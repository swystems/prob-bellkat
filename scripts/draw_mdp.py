#!/usr/bin/env python3
"""
Draw a MDP from a QBKAT JSON dump as a TikZ figure and, by default, an SVG.

Input
-----
The input JSON must contain an ``mdp_rendered`` field, like the JSON files
written under ``output/*.json``. If the JSON also contains ``extremal`` metadata,
the script uses it to mark the initial state and goal states.

Basic usage
-----------
    python scripts/draw_mdp.py output/quantPa.json

This writes, by default:
    output/figures/mdp/quantPa_mdp.tex
    output/figures/mdp/quantPa_mdp.svg
    output/figures/mdp/quantPa_mdp.layout.json

Useful variants
---------------
    # Only write TeX, without invoking LaTeX/dvisvgm.
    python scripts/draw_mdp.py output/quantPa.json --no-render

    # Ignore any previous coordinate cache and recompute the layout.
    python scripts/draw_mdp.py output/quantPa.json --no-reuse-layout

    # Make a wider drawing with more separation between sibling outcomes.
    python scripts/draw_mdp.py output/quantPa.json \
        --sibling-x-gap 3.4 --outcome-group-gap 1.4 --state-x-gap 3.2

    # Make the vertical layout looser.
    python scripts/draw_mdp.py output/quantPa.json \
        --state-y-gap 4.0 --action-y-offset 1.2 --label-y-offset 2.2

Layout model
------------
The drawing flows bottom-to-top. Back edges and loops are rendered as gray
reference copies instead of circular arrows. Outcome sibling sets reserve a
horizontal band, so sibling groups from different actions do not overlap. After
the top layer is fixed, a recursive top-down pass places each lower layer under
the action/outcome geometry above it.

Main customization knobs
------------------------
--state-x-gap         Minimum horizontal spacing between canonical states.
--state-y-gap         Vertical spacing between state layers.
--sibling-x-gap       Horizontal spacing between outcomes of one action.
--action-x-gap        Horizontal spacing between multiple enabled actions from
                      the same state.
--outcome-group-gap   Extra gap between whole sibling outcome groups.
--pointer-clearance   Extra gap used when gray reference nodes avoid occupied
                      row positions.
--probability-digits  Number of decimals shown on probability labels.
--tikz-font           TikZ font command, e.g. "\\scriptsize" or "\\footnotesize".
--node-inner-sep      TikZ inner sep for state and reference nodes, in pt.

Output rendering
----------------
SVG rendering uses ``latex`` followed by ``dvisvgm``. On macOS the script
prefers ``/Library/TeX/texbin`` when available, because smaller TeX installations
may not include the ``standalone`` class.
"""

import argparse
import json
import re
import shutil
import subprocess
from collections import Counter, defaultdict
from dataclasses import dataclass, field
from decimal import Decimal, InvalidOperation, ROUND_HALF_UP
from pathlib import Path


STATE_Y_GAP = 3.2
ACTION_Y_OFFSET = 1.0
LABEL_Y_OFFSET = 1.85
STATE_X_GAP = 2.8
SIBLING_X_GAP = 2.6
ACTION_X_GAP = 1.4
OUTCOME_GROUP_GAP = 0.9
POINTER_CLEARANCE = 0.18
PROBABILITY_DIGITS = 4
TIKZ_FONT = r"\scriptsize"
NODE_INNER_SEP = 2.1


@dataclass(frozen=True, order=True)
class State:
    pc: int
    multiset: tuple[str, ...]


@dataclass
class Outcome:
    target: State
    probability: Decimal
    probability_text: str
    cost_text: str


@dataclass
class Action:
    source: State
    action_index: int
    global_index: int
    outcomes: list[Outcome]
    cost_text: str | None = None


@dataclass
class MDP:
    initial_state: State
    goal_states: set[State]
    states: set[State]
    actions_by_source: dict[State, list[Action]]


@dataclass
class StateNode:
    node_id: str
    state: State
    x: float = 0.0
    y: float = 0.0
    layer: int = 0


@dataclass
class PointerNode:
    node_id: str
    state: State
    x: float
    y: float


@dataclass
class OutcomeDraw:
    outcome: Outcome
    label_id: str
    label_x: float
    label_y: float
    target_node_id: str


@dataclass
class ActionDraw:
    node_id: str
    action: Action
    x: float
    y: float
    outcomes: list[OutcomeDraw] = field(default_factory=list)


@dataclass
class Drawing:
    states: dict[State, StateNode]
    pointers: list[PointerNode]
    actions: list[ActionDraw]
    layers: dict[int, list[State]]


OUTCOME_RE = re.compile(
    r"\(\s*(?P<pc>\d+)\s*,\s*⦃(?P<mset>.*?)⦄\s*\)"
    r"\s*×\s*《\s*(?P<prob>[^,]+?)\s*,\s*(?P<cost>[^》]+?)\s*》"
)


def parse_args():
    parser = argparse.ArgumentParser(
        description="Draw a BellKAT MDP JSON dump as TikZ and SVG.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument("input_json", help="JSON dump containing mdp_rendered.")

    output_group = parser.add_argument_group("output")
    output_group.add_argument(
        "--output-dir",
        default="output/figures/mdp",
        help="Directory for the generated .tex, .svg, and layout files.",
    )
    output_group.add_argument(
        "--file-stem",
        default=None,
        help="Output file stem. Defaults to '<input-stem>_mdp'.",
    )
    output_group.add_argument(
        "--layout-json",
        default=None,
        help="Optional coordinate cache. Defaults to '<output-dir>/<file-stem>.layout.json'.",
    )
    output_group.add_argument(
        "--no-reuse-layout",
        action="store_true",
        help="Ignore an existing layout JSON cache.",
    )
    output_group.add_argument(
        "--no-save-layout",
        action="store_true",
        help="Do not write the final coordinate cache.",
    )
    output_group.add_argument(
        "--no-render",
        action="store_true",
        help="Only write the TikZ/TeX source, without compiling an SVG.",
    )
    output_group.add_argument(
        "--keep-pdf",
        action="store_true",
        help="Keep the intermediate PDF created while rendering the SVG.",
    )

    layout_group = parser.add_argument_group("layout")
    layout_group.add_argument(
        "--state-y-gap",
        type=float,
        default=STATE_Y_GAP,
        help="Vertical distance between consecutive state layers, in TikZ cm.",
    )
    layout_group.add_argument(
        "--action-y-offset",
        type=float,
        default=ACTION_Y_OFFSET,
        help="Vertical distance from a state to its enabled action nodes.",
    )
    layout_group.add_argument(
        "--label-y-offset",
        type=float,
        default=LABEL_Y_OFFSET,
        help="Vertical distance from a state row to outcome probability/cost labels.",
    )
    layout_group.add_argument(
        "--state-x-gap",
        type=float,
        default=STATE_X_GAP,
        help="Minimum horizontal distance between canonical state centers.",
    )
    layout_group.add_argument(
        "--sibling-x-gap",
        type=float,
        default=SIBLING_X_GAP,
        help="Horizontal spacing between outcomes of the same action.",
    )
    layout_group.add_argument(
        "--action-x-gap",
        type=float,
        default=ACTION_X_GAP,
        help="Horizontal spacing between multiple enabled actions of one state.",
    )
    layout_group.add_argument(
        "--outcome-group-gap",
        type=float,
        default=OUTCOME_GROUP_GAP,
        help="Extra spacing between whole outcome sibling groups on a layer.",
    )
    layout_group.add_argument(
        "--pointer-clearance",
        type=float,
        default=POINTER_CLEARANCE,
        help="Extra spacing used when placing gray reference nodes on occupied rows.",
    )

    rendering_group = parser.add_argument_group("rendering")
    rendering_group.add_argument(
        "--probability-digits",
        type=int,
        default=PROBABILITY_DIGITS,
        help="Number of decimal places used for probability labels.",
    )
    rendering_group.add_argument(
        "--tikz-font",
        default=TIKZ_FONT,
        help='TikZ font command, e.g. "\\scriptsize" or "\\footnotesize".',
    )
    rendering_group.add_argument(
        "--node-inner-sep",
        type=float,
        default=NODE_INNER_SEP,
        help="TikZ inner sep for state/reference nodes, in pt.",
    )
    return parser.parse_args()


def apply_cli_options(args):
    global STATE_Y_GAP
    global ACTION_Y_OFFSET
    global LABEL_Y_OFFSET
    global STATE_X_GAP
    global SIBLING_X_GAP
    global ACTION_X_GAP
    global OUTCOME_GROUP_GAP
    global POINTER_CLEARANCE
    global PROBABILITY_DIGITS
    global TIKZ_FONT
    global NODE_INNER_SEP

    STATE_Y_GAP = args.state_y_gap
    ACTION_Y_OFFSET = args.action_y_offset
    LABEL_Y_OFFSET = args.label_y_offset
    STATE_X_GAP = args.state_x_gap
    SIBLING_X_GAP = args.sibling_x_gap
    ACTION_X_GAP = args.action_x_gap
    OUTCOME_GROUP_GAP = args.outcome_group_gap
    POINTER_CLEARANCE = args.pointer_clearance
    PROBABILITY_DIGITS = args.probability_digits
    TIKZ_FONT = args.tikz_font
    NODE_INNER_SEP = args.node_inner_sep

    if PROBABILITY_DIGITS < 0:
        raise SystemExit("--probability-digits must be nonnegative.")


def load_json(path):
    with open(path, "r", encoding="utf-8") as handle:
        return json.load(handle)


def parse_state_payload(payload):
    return State(int(payload["pc"]), tuple(payload.get("bell_pairs", [])))


def parse_mdp_payload(payload):
    if "mdp_rendered" not in payload:
        raise SystemExit("The JSON file does not contain an 'mdp_rendered' entry.")

    extremal = payload.get("extremal", {})
    initial_payload = extremal.get("initial_state", {"pc": 0, "bell_pairs": []})
    initial_state = parse_state_payload(initial_payload)
    goal_states = {
        parse_state_payload(state_payload)
        for state_payload in extremal.get("goal_states", [])
    }
    known_states = {
        parse_state_payload(state_payload)
        for state_payload in extremal.get("states", [])
    }

    rendered_states, actions_by_source = parse_rendered_mdp(payload["mdp_rendered"])
    all_states = set(known_states) | rendered_states | {initial_state} | goal_states
    for actions in actions_by_source.values():
        for action in actions:
            for outcome in action.outcomes:
                all_states.add(outcome.target)

    return MDP(initial_state, goal_states, all_states, actions_by_source)


def parse_rendered_mdp(rendered):
    current_pc = None
    global_action_index = 1
    states = set()
    actions_by_source = defaultdict(list)

    for raw_line in rendered.splitlines():
        if not raw_line.strip():
            continue

        line = raw_line.rstrip()
        stripped = line.strip()
        pc_match = re.fullmatch(r"\^?(\d+):", stripped)
        if pc_match:
            current_pc = int(pc_match.group(1))
            continue

        if current_pc is None:
            raise SystemExit(f"Found a state line before any program counter: {line}")

        cleaned = stripped.removeprefix("^")
        if ":" not in cleaned:
            raise SystemExit(f"Could not parse MDP line: {line}")
        source_text, action_set_text = cleaned.split(":", 1)
        source = State(current_pc, parse_multiset_text(source_text.strip()))
        states.add(source)

        body = extract_action_body(action_set_text.strip())
        if not body:
            actions_by_source.setdefault(source, [])
            continue

        for local_index, action_text in enumerate(split_top_level(body, ","), start=1):
            outcomes = parse_action_outcomes(action_text)
            if not outcomes:
                continue
            costs = {outcome.cost_text for outcome in outcomes}
            action = Action(
                source=source,
                action_index=local_index,
                global_index=global_action_index,
                outcomes=outcomes,
                cost_text=next(iter(costs)) if len(costs) == 1 else None,
            )
            actions_by_source[source].append(action)
            global_action_index += 1
            states.update(outcome.target for outcome in outcomes)

    return states, dict(actions_by_source)


def extract_action_body(action_set_text):
    if "⦅" not in action_set_text or "⦆" not in action_set_text:
        raise SystemExit(f"Expected an action set delimited by ⦅...⦆, got: {action_set_text}")
    return action_set_text.split("⦅", 1)[1].rsplit("⦆", 1)[0].strip()


def parse_multiset_text(text):
    text = text.strip().removeprefix("^").strip()
    if text.startswith("⦃") and text.endswith("⦄"):
        text = text[1:-1]
    text = text.strip()
    if not text:
        return ()
    return tuple(part.strip() for part in text.split(",") if part.strip())


def split_top_level(text, separator):
    parts = []
    start = 0
    depth_round = 0
    depth_mset = 0
    depth_weight = 0

    for index, char in enumerate(text):
        if char == "(":
            depth_round += 1
        elif char == ")":
            depth_round -= 1
        elif char == "⦃":
            depth_mset += 1
        elif char == "⦄":
            depth_mset -= 1
        elif char == "《":
            depth_weight += 1
        elif char == "》":
            depth_weight -= 1
        elif (
            char == separator
            and depth_round == 0
            and depth_mset == 0
            and depth_weight == 0
        ):
            parts.append(text[start:index].strip())
            start = index + 1

    parts.append(text[start:].strip())
    return [part for part in parts if part]


def parse_action_outcomes(action_text):
    outcomes = []
    for outcome_text in split_top_level(action_text, "+"):
        match = OUTCOME_RE.fullmatch(outcome_text.strip())
        if not match:
            raise SystemExit(f"Could not parse outcome: {outcome_text}")
        probability_text = match.group("prob").strip()
        outcomes.append(
            Outcome(
                target=State(
                    int(match.group("pc")),
                    parse_multiset_text(f"⦃{match.group('mset')}⦄"),
                ),
                probability=parse_decimal(probability_text),
                probability_text=probability_text,
                cost_text=match.group("cost").strip(),
            )
        )
    return outcomes


def parse_decimal(text):
    try:
        return Decimal(text)
    except InvalidOperation as exc:
        raise SystemExit(f"Could not parse decimal probability '{text}'.") from exc


def load_layout_cache(path, reuse):
    if not reuse or not path.exists():
        return {}
    with open(path, "r", encoding="utf-8") as handle:
        payload = json.load(handle)
    return payload.get("states", {})


def save_layout_cache(path, drawing):
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "states": {
            state_key(node.state): {
                "x": round(node.x, 6),
                "y": round(node.y, 6),
                "layer": node.layer,
            }
            for node in drawing.states.values()
        }
    }
    with open(path, "w", encoding="utf-8") as handle:
        json.dump(payload, handle, indent=2, sort_keys=True)
        handle.write("\n")


def build_drawing(mdp, layout_cache):
    state_nodes = {}
    pointers = []
    action_draws = []
    layers = defaultdict(list)
    desired_x = defaultdict(list)
    outcome_group_bands = defaultdict(list)
    seen = set()
    pointer_count = 0
    action_count = 0
    label_count = 0

    initial_node = StateNode("s0", mdp.initial_state, layer=0, y=0.0)
    initial_node.x = cached_x(layout_cache, mdp.initial_state, default=0.0)
    state_nodes[mdp.initial_state] = initial_node
    layers[0].append(mdp.initial_state)
    seen.add(mdp.initial_state)

    depth = 0
    while depth in layers:
        assign_layer_coordinates(depth, layers[depth], state_nodes, desired_x, layout_cache)
        current_states = sorted(layers[depth], key=lambda state: state_nodes[state].x)

        for source in current_states:
            source_node = state_nodes[source]
            actions = mdp.actions_by_source.get(source, [])
            action_offsets = centered_offsets(len(actions), ACTION_X_GAP)

            for local_action_position, action in enumerate(actions):
                action_count += 1
                preferred_action_x = source_node.x + action_offsets[local_action_position]
                outcome_offsets = centered_offsets(len(action.outcomes), SIBLING_X_GAP)
                target_layer = depth + 1
                band_left, band_right = outcome_group_band(action, outcome_offsets)
                action_x = reserve_group_center(
                    preferred_action_x,
                    band_left,
                    band_right,
                    outcome_group_bands[target_layer],
                )
                action_y = source_node.y + ACTION_Y_OFFSET
                action_draw = ActionDraw(
                    node_id=f"a{action_count}",
                    action=action,
                    x=action_x,
                    y=action_y,
                )

                for outcome_position, outcome in enumerate(action.outcomes):
                    outcome_x = action_x + outcome_offsets[outcome_position]
                    target_y = source_node.y + STATE_Y_GAP

                    if outcome.target not in seen:
                        node_id = f"s{len(state_nodes)}"
                        state_nodes[outcome.target] = StateNode(
                            node_id=node_id,
                            state=outcome.target,
                            y=target_y,
                            layer=target_layer,
                        )
                        layers[target_layer].append(outcome.target)
                        desired_x[outcome.target].append(outcome_x)
                        seen.add(outcome.target)
                        target_node_id = node_id
                    else:
                        pointer_count += 1
                        pointer = PointerNode(
                            node_id=f"r{pointer_count}",
                            state=outcome.target,
                            x=outcome_x,
                            y=target_y,
                        )
                        pointers.append(pointer)
                        target_node_id = pointer.node_id

                    label_count += 1
                    action_draw.outcomes.append(
                        OutcomeDraw(
                            outcome=outcome,
                            label_id=f"l{label_count}",
                            label_x=outcome_x,
                            label_y=source_node.y + LABEL_Y_OFFSET,
                            target_node_id=target_node_id,
                        )
                    )

                action_draws.append(action_draw)

        depth += 1

    propagate_top_down_alignment(state_nodes, pointers, action_draws, layers)
    return Drawing(dict(state_nodes), pointers, action_draws, dict(layers))


def assign_layer_coordinates(depth, states, state_nodes, desired_x, layout_cache):
    unresolved = []
    for state in states:
        node = state_nodes[state]
        node.y = depth * STATE_Y_GAP
        if state_key(state) in layout_cache:
            node.x = float(layout_cache[state_key(state)].get("x", node.x))
        elif desired_x.get(state):
            node.x = sum(desired_x[state]) / len(desired_x[state])
        else:
            unresolved.append(state)

    for index, state in enumerate(sorted(unresolved, key=semantic_state_key)):
        state_nodes[state].x = (index - (len(unresolved) - 1) / 2) * STATE_X_GAP

    ordered = sorted(states, key=lambda state: (state_nodes[state].x, semantic_state_key(state)))
    enforce_min_gap(ordered, state_nodes, STATE_X_GAP)


def outcome_group_band(action, outcome_offsets):
    if not action.outcomes:
        return -0.5, 0.5

    left = 0.0
    right = 0.0
    for outcome, offset in zip(action.outcomes, outcome_offsets):
        half_width = max(estimated_half_width(outcome.target), estimated_edge_label_half_width(outcome))
        left = min(left, offset - half_width)
        right = max(right, offset + half_width)
    return left, right


def reserve_group_center(preferred_center, band_left, band_right, occupied_bands):
    interval = (preferred_center + band_left, preferred_center + band_right)
    if not interval_overlaps_any(interval, occupied_bands):
        occupied_bands.append(interval)
        occupied_bands.sort()
        return preferred_center

    center = max(
        preferred_center,
        max(occupied_right + OUTCOME_GROUP_GAP - band_left for _occupied_left, occupied_right in occupied_bands),
    )
    for _ in range(len(occupied_bands) + 8):
        interval = (center + band_left, center + band_right)
        if not interval_overlaps_any(interval, occupied_bands):
            occupied_bands.append(interval)
            occupied_bands.sort()
            return center
        center += STATE_X_GAP

    raise SystemExit("Could not place an action outcome group without overlap.")


def interval_overlaps_any(interval, occupied_bands):
    left, right = interval
    for occupied_left, occupied_right in occupied_bands:
        if left < occupied_right + OUTCOME_GROUP_GAP and right > occupied_left - OUTCOME_GROUP_GAP:
            return True
    return False


def propagate_top_down_alignment(state_nodes, pointers, action_draws, layers):
    actions_by_source = defaultdict(list)
    state_nodes_by_id = {node.node_id: node for node in state_nodes.values()}
    pointers_by_id = {pointer.node_id: pointer for pointer in pointers}

    for action_draw in action_draws:
        actions_by_source[action_draw.action.source].append(action_draw)

    if not layers:
        return

    max_depth = max(layers)
    for target_depth in range(max_depth, 0, -1):
        relax_pointer_rows(pointers, state_nodes)
        source_depth = target_depth - 1

        for state in sorted(layers.get(source_depth, []), key=lambda item: state_nodes[item].x):
            outgoing_actions = actions_by_source.get(state, [])
            if not outgoing_actions:
                continue

            for action_draw in outgoing_actions:
                target_xs = []
                for outcome_draw in action_draw.outcomes:
                    target_x = draw_node_x(outcome_draw.target_node_id, state_nodes_by_id, pointers_by_id)
                    outcome_draw.label_x = target_x
                    target_xs.append(target_x)
                if target_xs:
                    action_draw.x = sum(target_xs) / len(target_xs)

            state_nodes[state].x = sum(action.x for action in outgoing_actions) / len(outgoing_actions)

    relax_pointer_rows(pointers, state_nodes)


def draw_node_x(node_id, state_nodes_by_id, pointers_by_id):
    if node_id in state_nodes_by_id:
        return state_nodes_by_id[node_id].x
    return pointers_by_id[node_id].x


def enforce_min_gap(states, state_nodes, gap):
    if not states:
        return

    original_center = sum(state_nodes[state].x for state in states) / len(states)
    last_x = None
    for state in states:
        if last_x is not None and state_nodes[state].x < last_x + gap:
            state_nodes[state].x = last_x + gap
        last_x = state_nodes[state].x

    new_center = sum(state_nodes[state].x for state in states) / len(states)
    shift = original_center - new_center
    for state in states:
        state_nodes[state].x += shift


def enforce_variable_gap(states, state_nodes):
    if not states:
        return

    last_right = None
    for state in states:
        half_width = estimated_half_width(state)
        if last_right is not None and state_nodes[state].x - half_width < last_right + 0.35:
            state_nodes[state].x = last_right + 0.35 + half_width
        last_right = state_nodes[state].x + half_width


def relax_pointer_rows(pointers, state_nodes):
    by_y = defaultdict(list)
    for pointer in pointers:
        by_y[round(pointer.y, 6)].append(pointer)

    for y, row_pointers in by_y.items():
        occupied = [
            (node.x, estimated_half_width(node.state))
            for node in state_nodes.values()
            if round(node.y, 6) == y
        ]
        occupied.sort()
        for pointer in sorted(row_pointers, key=lambda item: item.x):
            half_width = estimated_half_width(pointer.state)
            candidate = nearest_free_x(pointer.x, half_width, occupied)
            pointer.x = candidate
            occupied.append((candidate, half_width))
            occupied.sort()


def nearest_free_x(initial_x, half_width, occupied):
    if not occupied:
        return initial_x

    clearance = POINTER_CLEARANCE
    intervals = sorted(
        (
            occupied_x - occupied_half_width - half_width - clearance,
            occupied_x + occupied_half_width + half_width + clearance,
        )
        for occupied_x, occupied_half_width in occupied
    )
    merged = []
    for low, high in intervals:
        if not merged or low > merged[-1][1]:
            merged.append([low, high])
        else:
            merged[-1][1] = max(merged[-1][1], high)

    for low, high in merged:
        if low <= initial_x <= high:
            nudge = 0.03
            candidates = [low - nudge, high + nudge]
            return min(candidates, key=lambda candidate: (abs(candidate - initial_x), abs(candidate)))
    return initial_x


def estimated_half_width(state):
    if not state.multiset:
        return 0.36

    counts = Counter(state.multiset)
    rows = []
    for pair in dict.fromkeys(state.multiset):
        row = pair
        count = counts[pair]
        if count > 1:
            row += f"x{count}"
        rows.append(row)

    max_chars = max(len(row) for row in rows)
    return min(1.05, max(0.48, 0.28 + max_chars * 0.07))


def estimated_edge_label_half_width(outcome):
    probability = format_probability(outcome.probability)
    label_chars = len(probability) + len(outcome.cost_text) + 2
    return min(0.95, max(0.45, label_chars * 0.055))


def centered_offsets(count, gap):
    if count <= 0:
        return []
    return [(index - (count - 1) / 2) * gap for index in range(count)]


def cached_x(layout_cache, state, default):
    cached = layout_cache.get(state_key(state))
    if cached is None:
        return default
    return float(cached.get("x", default))


def semantic_state_key(state):
    return (semantic_position(state), state.pc, len(state.multiset), state.multiset)


def semantic_position(state):
    if not state.multiset:
        return 0.0
    return sum(pair_position(pair) for pair in state.multiset) / len(state.multiset) + state.pc * 0.1


def pair_position(pair):
    parts = re.split(r"[~=]", pair)
    if len(parts) != 2:
        return float(sum(ord(char) for char in pair)) / 100.0
    left, right = parts
    return (endpoint_position(left) + endpoint_position(right)) / 2.0


def endpoint_position(endpoint):
    endpoint = endpoint.strip()
    if len(endpoint) == 1 and endpoint.isalpha():
        return float(ord(endpoint.upper()) - ord("A"))
    return float(sum(ord(char) for char in endpoint)) / 100.0


def state_key(state):
    return f"{state.pc}|{','.join(state.multiset)}"


def write_tikz(tex_path, drawing, mdp):
    tex_path.parent.mkdir(parents=True, exist_ok=True)
    with open(tex_path, "w", encoding="utf-8") as handle:
        handle.write(render_tikz_document(drawing, mdp))


def render_tikz_document(drawing, mdp):
    lines = [
        r"\documentclass[tikz,border=6pt]{standalone}",
        r"\usepackage[T1]{fontenc}",
        r"\usepackage{amsmath}",
        r"\usepackage{xcolor}",
        r"\usetikzlibrary{arrows.meta,shapes.geometric}",
        r"\definecolor{pcColor}{RGB}{33,91,137}",
        r"\definecolor{msetColor}{RGB}{94,55,132}",
        r"\definecolor{probColor}{RGB}{21,104,92}",
        r"\definecolor{costColor}{RGB}{156,91,36}",
        r"\definecolor{goalFill}{RGB}{220,242,225}",
        r"\begin{document}",
        r"\begin{tikzpicture}[",
        r"  x=1cm, y=1cm,",
        r"  >=Stealth,",
        rf"  font={TIKZ_FONT},",
        rf"  state/.style={{draw, rounded corners=2pt, fill=white, align=center, inner sep={NODE_INNER_SEP}pt}},",
        r"  initState/.style={state, double, very thick},",
        r"  goalState/.style={state, fill=goalFill},",
        r"  initGoalState/.style={goalState, double, very thick},",
        r"  ptr/.style={state, draw=gray!55, text=gray!70, fill=gray!8},",
        r"  action/.style={diamond, draw, fill=white, aspect=1.45, inner sep=1.2pt},",
        r"  edgeLabel/.style={fill=white, inner sep=1pt}",
        r"]",
    ]

    for node in sorted(drawing.states.values(), key=lambda item: item.node_id):
        style = canonical_state_style(node.state, mdp)
        lines.append(
            rf"\node[{style}] ({node.node_id}) at ({fmt_coord(node.x)},{fmt_coord(node.y)}) "
            rf"{{{state_latex(node.state)}}};"
        )

    for pointer in drawing.pointers:
        lines.append(
            rf"\node[ptr] ({pointer.node_id}) at ({fmt_coord(pointer.x)},{fmt_coord(pointer.y)}) "
            rf"{{{state_latex(pointer.state)}}};"
        )

    for action_draw in drawing.actions:
        action = action_draw.action
        lines.append(
            rf"\node[action] ({action_draw.node_id}) at ({fmt_coord(action_draw.x)},{fmt_coord(action_draw.y)}) "
            rf"{{$\mathrm{{Act}}={action.action_index}$}};"
        )
        lines.append(rf"\draw[->] ({drawing.states[action.source].node_id}) -- ({action_draw.node_id});")

        for outcome_draw in action_draw.outcomes:
            label = edge_label_latex(outcome_draw.outcome)
            lines.append(
                rf"\node[edgeLabel] ({outcome_draw.label_id}) at "
                rf"({fmt_coord(outcome_draw.label_x)},{fmt_coord(outcome_draw.label_y)}) "
                rf"{{{label}}};"
            )
            lines.append(
                rf"\draw[->, densely dotted] ({action_draw.node_id}) -- "
                rf"({outcome_draw.label_id}) -- ({outcome_draw.target_node_id});"
            )

    lines.extend([r"\end{tikzpicture}", r"\end{document}", ""])
    return "\n".join(lines)


def canonical_state_style(state, mdp):
    is_initial = state == mdp.initial_state
    is_goal = state in mdp.goal_states
    if is_initial and is_goal:
        return "initGoalState"
    if is_initial:
        return "initState"
    if is_goal:
        return "goalState"
    return "state"


def state_latex(state):
    pc = tex_escape(str(state.pc))
    return r"$" + multiset_latex(state.multiset, pc) + r"$"


def multiset_latex(multiset, pc):
    pc_subscript = rf"_{{\color{{pcColor}}\scriptstyle {pc}}}"
    if not multiset:
        return color_mset(r"\emptyset") + pc_subscript

    counts = Counter(multiset)
    items = []
    for pair in dict.fromkeys(multiset):
        pair_tex = pair_latex(pair)
        count = counts[pair]
        if count == 1:
            items.append(pair_tex)
        else:
            items.append(rf"{pair_tex}^{{\times {count}}}")

    if len(items) == 1:
        return color_mset(r"\{\!\!\{" + items[0] + r"\}\!\!\}") + pc_subscript

    rows = []
    for index, item in enumerate(items):
        suffix = r"," if index < len(items) - 1 else ""
        rows.append(item + suffix)

    rendered_rows = [color_mset(r"\{\!\!\{" + rows[0])]
    rendered_rows.extend(color_mset(row) for row in rows[1:-1])
    rendered_rows.append(color_mset(rows[-1] + r"\}\!\!\}") + pc_subscript)
    return r"\begin{array}{@{}c@{}}" + r"\\[-1pt]".join(rendered_rows) + r"\end{array}"


def color_mset(content):
    return r"{\color{msetColor}" + content + r"}"


def pair_latex(pair):
    if "~" in pair:
        left, right = pair.split("~", 1)
        sep = r"{\sim}"
    elif "=" in pair:
        left, right = pair.split("=", 1)
        sep = "="
    else:
        return rf"\mathrm{{{tex_escape(pair)}}}"
    return rf"\mathrm{{{tex_escape(left)}{sep}{tex_escape(right)}}}"


def edge_label_latex(outcome):
    probability = format_probability(outcome.probability)
    cost = tex_escape(outcome.cost_text)
    return (
        r"$"
        rf"{{\color{{probColor}}{tex_escape(probability)}}}"
        r",\,"
        rf"{{\color{{costColor}}{cost}}}"
        r"$"
    )


def format_probability(probability):
    quantizer = Decimal(1).scaleb(-PROBABILITY_DIGITS)
    quantized = probability.quantize(quantizer, rounding=ROUND_HALF_UP)
    text = format(quantized, "f")
    if "." in text:
        text = text.rstrip("0").rstrip(".")
    return text or "0"


def tex_escape(text):
    replacements = {
        "\\": r"\textbackslash{}",
        "_": r"\_",
        "%": r"\%",
        "&": r"\&",
        "#": r"\#",
        "{": r"\{",
        "}": r"\}",
        "$": r"\$",
    }
    return "".join(replacements.get(char, char) for char in text)


def fmt_coord(value):
    return f"{value:.3f}".rstrip("0").rstrip(".")


def render_svg(tex_path, keep_pdf=False):
    latex = preferred_tex_binary("latex")
    pdflatex = preferred_tex_binary("pdflatex")
    if latex is None:
        raise SystemExit("latex is required to render the SVG, but it was not found.")
    if shutil.which("dvisvgm") is None:
        raise SystemExit("dvisvgm is required to render the SVG, but it was not found.")

    run_checked(
        [latex, "-interaction=nonstopmode", "-halt-on-error", tex_path.name],
        cwd=tex_path.parent,
    )
    dvi_path = tex_path.with_suffix(".dvi")
    svg_path = tex_path.with_suffix(".svg")

    dvisvgm_command = [
        "dvisvgm",
        "--exact-bbox",
        "--font-format=woff",
        dvi_path.name,
        "-o",
        svg_path.name,
    ]
    ghostscript = shutil.which("gs")
    if ghostscript is not None:
        dvisvgm_command.insert(1, f"--libgs={ghostscript}")
    run_checked(dvisvgm_command, cwd=tex_path.parent)

    pdf_path = tex_path.with_suffix(".pdf")
    if keep_pdf:
        if pdflatex is None:
            raise SystemExit("pdflatex is required to keep a PDF, but it was not found.")
        run_checked(
            [pdflatex, "-interaction=nonstopmode", "-halt-on-error", tex_path.name],
            cwd=tex_path.parent,
        )

    for suffix in [".aux", ".log", ".dvi"]:
        aux_path = tex_path.with_suffix(suffix)
        if aux_path.exists():
            aux_path.unlink()
    if not keep_pdf and pdf_path.exists():
        pdf_path.unlink()

    return svg_path


def preferred_tex_binary(name):
    candidates = [Path("/Library/TeX/texbin") / name]
    found = shutil.which(name)
    if found is not None:
        candidates.append(Path(found))

    for candidate in candidates:
        if candidate.exists():
            return str(candidate)
    return None


def run_checked(command, cwd):
    result = subprocess.run(command, cwd=cwd, text=True, capture_output=True)
    if result.returncode != 0:
        joined = " ".join(command)
        raise SystemExit(
            f"Command failed: {joined}\n\nSTDOUT:\n{result.stdout}\n\nSTDERR:\n{result.stderr}"
        )


def main():
    args = parse_args()
    apply_cli_options(args)
    input_path = Path(args.input_json)
    output_dir = Path(args.output_dir)
    file_stem = args.file_stem or f"{input_path.stem}_mdp"
    layout_path = Path(args.layout_json) if args.layout_json else output_dir / f"{file_stem}.layout.json"
    tex_path = output_dir / f"{file_stem}.tex"

    mdp = parse_mdp_payload(load_json(input_path))
    layout_cache = load_layout_cache(layout_path, reuse=not args.no_reuse_layout)
    drawing = build_drawing(mdp, layout_cache)

    write_tikz(tex_path, drawing, mdp)
    if not args.no_save_layout:
        save_layout_cache(layout_path, drawing)

    print(f"Wrote {tex_path}")
    if not args.no_save_layout:
        print(f"Wrote {layout_path}")
    if not args.no_render:
        svg_path = render_svg(tex_path, keep_pdf=args.keep_pdf)
        print(f"Wrote {svg_path}")


if __name__ == "__main__":
    main()
