---
title: Artifact for "A Language for Quantifying Quantum Network Behavior"
toc: true
colorlinks: true
urlcolor: blue!50!black
toccolor: green!30!black
header-includes: |
    \usepackage{newunicodechar}
    \newfontfamily{\fallbackfont}{Symbola}
    \DeclareTextFontCommand{\textfallback}{\fallbackfont}
    \newunicodechar{⦅}{\textfallback{⦅}}
    \newunicodechar{⦆}{\textfallback{⦆}}
    \newunicodechar{⦄}{\textfallback{⦄}}
    \newunicodechar{⦃}{\textfallback{⦃}}
    \newunicodechar{▶}{\textfallback{▶}}
    \newunicodechar{⊤}{\textfallback{⊤}}
    \newunicodechar{∧}{\textfallback{∧}}
    \newunicodechar{∨}{\textfallback{∨}}
---

The PBKAT tool allows to analyze behaviors of quantum network protocols capturing both probabilistic behavior stemming from quantum mechanics and non-determinstic behavior arising from resource contention. The tool is based on a [Haskell][haskell] library `bellkat` plus many examples provided as executables within the same [Haskell][haskell] package.

The PBKAT tool can:

  * produce automata capturing guarded strings of sets via `automaton` command
  * produce the execution traces via `execution-trace` command (e.g., Fig. 5 and Fig. 11)
  * produce the convex set of probability distributions via `run` command (Table 1)
     * these outputs can be compared for equality to check if different protocols are equivalent
       with respect to $[\![-]\!]$ semantics
     * machine-readable form for the next step is supported via `--json` option 
  * analyze the produced convex set of distributions via `probability` command (Table 1)


We provide two build options: [Nix][nix]-based and [Stack][stack]-based (details are in [Reusability guide](#reusability-guide)). 
For reproducibility we will explain how to use a `Dockerfile` with a ready-to-use [Nix][nix]-based environment.

**Docker note:** please, be aware that `docker run` commands may have to be prefixed with `sudo`,
depending on the docker setup (see details [here][docker-run]).

# Hardware Dependencies

 * RAM: up to 10 GB

# Getting Started Guide

"Getting Started" guide presents only the simplest way to get started with the artifact, namely, using [Docker][docker].
Instructions to create proper development environments, namely using either standard Haskell's [stack][stack] tool or [nix][nix] package manager are given in [Development Environment](#development-environments) section.

## Software requirements

  * [Docker][docker]

## Docker container setup

To create a docker container: 

  * Change to artifact's root
  * Create a [Docker][docker] container by running

    ```bash
    docker build --tag pbkat:latest .
    ```

To enter the container environment while making the current directory exposed from within the container:

```bash
docker run --rm -it --mount type=bind,source=$(pwd),target=/opt/pbkat pbkat:latest
```

**!!!** Henceforth and until [Reusability Guide](#reusability-guide) it is assumed that all actions are performed from within the container.


## Preparing the environment

 1. Generate build info:

    ```bash
    hpack
    ```

 2. Build the tool:

    ```bash
    cabal build
    ```

 3. Test the tool:

    ```bash
    cabal test --test-options="--skip=VERYLONG"
    ```

## Running a simple protocol

To check that everything works as expected we produce a somewhat trivial output for the protocol \S 3(a) using both PBKAT and BellKAT.

_Generally probabilistic "versions" will be preceded by `prob` in the
protocol names_

**PBKAT** generating a convex set of distributions as per example **I** from Section 5.1 (parallel
version), modulo
fractional/decimal notation and some formatting for readability:

```bash
cabal run probP5_1_I_parallel run
# ⦅⦃⦄×127 % 1000+⦃A~C⦄×117 % 250+⦃A~C,B~C⦄×81 % 250+⦃B~C⦄×81 % 1000,
#  ⦃⦄×181 % 1000+⦃A~C⦄×81 % 250+⦃A~C,B~C⦄×81 % 250+⦃B~C⦄×171 % 1000⦆
```

PBKAT output notation: 

  * `⦅...⦆` denotes a convex set (represented by generators)
  * `x1 × p1, x2 × p2, ..., xk × pk` represents a probability distribution over a set
    $\{\texttt{x1}, \texttt{x2}, \ldots, \texttt{xk}\}$ with corresponding probabilities `p1`, `p2`,
    ..., `pk`.
  * `⦃...⦄` denotes a _multi_ set
  * `A~C` denotes a Bell pair as in the paper
  * `x % y` denotes rational number $\frac{x}{y}$

**BellKAT** generating a set of possible outputs for the same example:

```bash
cabal run P5_1_I_parallel run
# [[],[["A","C"]],[["A","C"],["B","C"]],[["B","C"]]]
```

BellKAT output notation: 

  * `["A", "B"]` denotes a Bell pair $A \sim B$
  * `[[...], [...], ...]` denotes a multiset
  * `[[[...], ...], [[...], ...], ...]` denotes a set of multisets


# Step-by-step Guide

Before diving into the exact steps required to reproduce the results, we explain the inputs to the
tool and in a bit more detail the embedded DSL for protocol specification. 

## Inputs to the tool

Here's an overview of how three pieces of the inputs are captured in a tool with concrete
examples corresponding  the simple protocol analyzed in [Getting Started](#getting-started) (see
`probabilistic-examples/Pa.hs`):

  * **protocol specification (PBKAT expression)**: embedded DSL (see [Embedded
    DSL](#embedded-dsl))

    Example:
    ```haskell
    p :: ProbBellKATPolicy
    p = (create "C" <||> create "C") 
        <> 
        (trans "C" ("A", "C") <||> trans "C" ("B", "C"))
        <>
        (swap "C" ("A", "B"))
    ```

  * probabilities of basic actions: record of `ProbabilisticActionConfiguration`

      * `pacTransmitProbability` holds probabilities of successful transmission

      * `pacCreateProbability` holds probabilities of successful creation at individual locations

      * `pacUCreateProbability` holds probabilities of successful creation of an already distributed
        Bell pair (used extensively in case studies)

      * `pacSwapProbability` holds probabilities of successful swap at individual locations

    Example:
    ```haskell
    actionConfig = PAC 
        { pacTransmitProbability = 
            [(("C", "A"), 8/10)
            ,(("C", "B"), 7/10)
            ]
        , pacCreateProbability = [("C", 9/10)]
        , pacSwapProbability = [("C", 6/10)]
        , pacUCreateProbability = []
        }
    ```

  * network capacity: record of `NetworkCapacity BellKATTag`
  * desired output state: record of `BellPairsPredicate BellKATTag` built using tests (see [Embedded
    DSL](#embedded-dsl))

    Example:
    ```haskell
        ev = "A" ~~? "B"
    ```

## Embedded DSL

Bell pairs (e.g., to specify initial states):

 * $A \sim B$ is represented by `"A" ~ "B"`

Basic actions:

  * $cr\langle X \rangle$ is represented by `create "X"`
  * $tr\langle X \rightarrow Y \sim Z \rangle$ is represented by `trans "X" ("Y", "Z")`
  * $sw\langle X \sim Y @ Z \rangle$ is represented by `swap "Z" ("X", "Y")`
  * $di\langle X \sim Y\rangle$ is represented by `distill ("X", "Y")`
  * $\emptyset \triangleright X \sim Y$ is represented by `ucreate ("X", "Y")`

Operations:

  * sequential composition is represented by `<>`
  * parallel composition $||$ is represented by `<||>`
  * ordered composition $\circ$ is represented by `<.>`
  * conditional $+_\alpha$ is represented by `ite t`, where

     * `t` is the test $\alpha$
  * _bounded_ loop $(\square +_\alpha 1)^{(n)}$ is represented by `whileN n t`, where 

     * `n` is the number of iterations $n$
     * `t` is the test $\alpha$

    As per "Data-Availability Statement" the tool does not support unbounded while loops.

Tests:

  * checking absence of $\{\!\{X \sim Y\}\!\}$ is represented by `test ("X" /~? "Y")`
  * checking presence of $\{\!\{X \sim Y\}\!\}$ is represented by `test ("X" ~~? "Y")`

## Reproducing results from the paper

In what follows we explain how to use the tool to produce three kinds of results: convex sets of
distributions capturing the meaning of entanglement distribution protocols, automata capturing the
guarded sets of strings representing abstract semantics, and how to produce execution traces in the
form of transition systems.

### Computing convex sets and measuring performance (Table 1)

Each PBKAT row of the table shows the results for a specific protocol with name
`PROTO` corresponding to a source file `probabilistic-examples/PROTO.hs`. 

#### Correspondence table 

Below we give a table of correspondence between the protocol names in Table 1 and the values of
`PROTO`.

|**Protocol**                       |`PROTO`                         |
|-----------------------------------|--------------------------------|
|\S 3($a$)                          |`Pa`                            |
|\S 3($a_1$)                        |`Pa1`                           |
|Ex. 4.2                            |`Pag`                           |
|Ex. 5.1(I), $\circ$                |`P5_1_I_ordered`                |
|Ex. 5.1(I), $||$                   |`P5_1_I_parallel`               |
|Ex. 5.1(II), $\circ$, 2 iter.      |`P5_1_II_ordered`               |
|Ex. 5.1(II), $||$, 2 iter.         |`P5_1_II_parallel`              |
|Ex. 5.1(II), $\circ$, 3 iter.      |`P5_1_II_ordered_three`         |
|Ex. 5.1(II), $||$, 3 iter.         |`P5_1_II_parallel_three`        |
|Ex. 5.1(III), 1 iter.              |`P5_1_III_one`                  |
|Ex. 5.1(III), 2 iter.              |`P5_1_III_two`                  |
|Ex. 5.1(IV), 3 iter.               |`P5_1_IV`                       |
|\S 5.3(sw)                         |`P5_3_pompili`                   |
|\S 5.3(di), outer                  |`P5_3_coopmans_outer`           |
|\S 5.3(di), inner                  |`P5_3_coopmans_inner`           |
|\S 5.3(di), mixed                  |`P5_3_coopmans_mixed`           |

#### Generating the results as a pdf

The information for Table 1 can all be automatically generated and formatted into a `.tex` file using the `collect_stats.py` script.

```bash
# the command takes long time up to complete (~ 10 mins)
# one can pass --fast to skip the last protocol (~ 1 min)
./collect_stats.py --tex --standalone >results.tex 
```

Which can then be transformed into `results.pdf` with (also within the container):

```bash
pdflatex results.tex
```

**!** Since the container mounted the current folder, the `results.pdf` can be opened from the host.

Structure of the generate PDF is the following:

  * Section name with name of the protocol

  * **Output** showing a convex set of distributions over multisets of Bell pairs for
    PBKAT

  * **Stats** with the PBKAT statistics that goes into columns of Table 1:

      * Num Generators: $|O|$
      * Success probability: $p(\textbf{Goal})$
      * Memory (MiB): **Memory**
      * Time (s): **Time**

  * If there is a BellKAT version of the protocol there are two more paragraphs:

      * **BellKAT Output** showing a set of possible multisets of BellPairs
      * **BellKAT Stats** with the BellKAT statistics that goes into columns of Table 1 similar to that of
        PBKAT

#### Checking protocol equivalence

To check if the two protocols have the same semantics, it is enough to compare the generated convex
sets. As an example we look at PBKAT protocol `P5_1_II_parallel_three` and try comparing it with
a different, but equivalent version `P5_1_II_parallel_three_alt`:

```bash
cabal run probP5_1_II_parallel_three -- \
    run >probP5_1_II_parallel_three.txt
cabal run probP5_1_II_parallel_three_alt -- \
    run >probP5_1_II_parallel_three_alt.txt
diff probP5_1_II_parallel_three.txt probP5_1_II_parallel_three_alt.txt
```

#### Manual workflow (optional)

The workflow for getting results in Table 1 is the following:

 1. The output, i.e., the convex set of distributions over multisets of Bell pairs represented in the table as a number of generators in column $|O|$, and the performance statistics (columns **Memory** and **Time**) is gathered with

    ```bash
    cabal run probPROTO -- +RTS --machine-readable -t -RTS --json \
        run >PROTO.json 2>PROTO.json.stderr
    ```

     * output goes into `PROTO.json`
     * statistics goes into `PROTO.json.stderr`

 2. Probability $p(\textbf{Goal})$ of successfully generating the required set of Bell
    pairs specified in **Goal** column is computed via

    ```bash
    cabal run probPROTO -- probability <PROTO.json
    ```

#### Make-automated workflow (optional)

All the experimental results can be generated automatically as follows


```bash
# the command takes long time up to complete (~ 10 mins)
# one can pass FAST to skip the last protocol (~ 1 min)
make all-prob
```

**!** the outputs will be stored in `output/probabilistic/examples` directory.

### Reproducing the execution traces

The general way to produce execution traces for protocol `PROTO` is to execute:

```bash
cabal run probPROTO execution-trace
```

**Example.** To analyze protocol \S 3($a$) in the paper, we can set `PROTO` to `Pa` (see [table](#correspondence-table)), hence running

```bash
cabal run probPa execution-trace
```

The corresponding output will be (if formatted for readability):


```
^0:
  ^⦃⦄: ⦅(1,⦃⦄)×1 % 100+(1,⦃C~C⦄)×9 % 50+(1,⦃C~C,C~C⦄)×81 % 100⦆
1:
  ⦃⦄: ⦅(2,⦃⦄)⦆
  ⦃C~C⦄: ⦅(2,⦃⦄)×1 % 5+(2,⦃A~C⦄)×4 % 5,(2,⦃⦄)×3 % 10+(2,⦃B~C⦄)×7 % 10⦆
  ⦃C~C,C~C⦄: ⦅(2,⦃⦄)×3 % 50+(2,⦃A~C⦄)×6 % 25
              +(2,⦃A~C,B~C⦄)×14 % 25+(2,⦃B~C⦄)×7 % 50⦆
2:
  ⦃⦄: ⦅(3,⦃⦄)⦆
  ⦃A~C⦄: ⦅(3,⦃A~C⦄)⦆
  ⦃A~C,B~C⦄: ⦅(3,⦃⦄)×2 % 5+(3,⦃A~B⦄)×3 % 5⦆
  ⦃B~C⦄: ⦅(3,⦃B~C⦄)⦆
3:
  ⦃⦄: ⦅⦆
  ⦃A~B⦄: ⦅⦆
  ⦃A~C⦄: ⦅⦆
  ⦃B~C⦄: ⦅⦆
```

#### Explanation of the output

The execution trace is _essentially a transition system_ with each state identified by a pair $(s,
a)$, where $a$ a multiset of Bell pairs representing network state similar to presentation in the
paper, and $s$ is an additional identifier similar to a program counter, e.g., $s$ is what
distinguishes different occurrences of $\{\!\{B\sim C\}\!\}$ in Fig. 5.

The output has the following meaning in relation to the figures in the paper:

  * `n:`---signifies the start of a block for a given $s$ with `^` meaning _"initial"_.
  * `  ⦃...⦄:`---marks the transitions from the state $(s, a)$ (`⦃...⦄` is $a$)
  * `⦅...⦆` are transitions, essentially is a convex set of distributions over states

     * each generator of the convex set corresponds to a separate $\Delta$ (non-determinism, solid line in figures)

       **E.g.** state $(1, \{\!\{C\sim C\}\!\})$ has two outgoing edges: $\Delta_2$ and $\Delta_2'$

     * the distribution within each generator is $\Delta$ itself (probabilistic choice, dashed
       lines in figures)

       **E.g.** $\Delta_2$ has two probabilistic transitions, namely, to $\{\!\{A\sim C\}\!\}$ with probability $0.8$ and to $\emptyset$ with probability $0.2$ (both with $s = 2$).

#### Examples from the paper

Specific execution traces from the paper can be generated with an appropriate choice of the protocol
`PROTO`:

 * Figure 5

     * protocol ($a_1$): `PROTO` is `Pa1`
     * protocol ($a$): `PROTO` is `Pa`


 * Trace at the end of Section 4.2: `PROTO` is `P4`

 * Figure 11: `PROTO` is `probP5_1_IV`

    * presented steps correspond to states `2` through `9`
    * some simplifications are performed over the traces to fit it into (essentially by identifying equivalent states)

### Generating automata capturing guarded strings of sets

The general way to produce an automaton for protocol `PROTO` is to execute:

```bash
cabal run probPROTO automaton
```

**Example.** To analyze Example 4.2 in the paper, we set `PROTO` to `P4` hence running

```bash
cabal run probP4 automaton
```

The corresponding output will be (slightly formatted for readability):

```
^0:
[¬C~C]-( {[⊤]⦃⦄▶⦃⦄×1 % 9+⦃C~C⦄×4 % 9+⦃C~C,C~C⦄×4 % 9} )-> 1
[C~C]-( {[⊤]⦃C~C,C~C⦄▶⦃⦄×1 % 25+⦃A~C⦄×8 % 25+⦃A~C,A~C⦄×16 % 25,
         [⦃C~C⦄]⦃⦄▶⦃⦄,[⦃C~C,C~C⦄]⦃C~C⦄▶⦃⦄×1 % 5+⦃A~C⦄×4 % 5} )-> 2
1: [⊤]-( {[⊤]⦃C~C,C~C⦄▶⦃⦄×1 % 10+⦃A~C⦄×2 % 5+⦃A~C,B~C⦄×2 % 5+⦃B~C⦄×1 % 10,
          [⦃C~C⦄]⦃⦄▶⦃⦄,
          [⦃C~C,C~C⦄]⦃C~C⦄▶⦃⦄×1 % 5+⦃A~C⦄×4 % 5,
          [⦃C~C,C~C⦄]⦃C~C⦄▶⦃⦄×1 % 2+⦃B~C⦄×1 % 2} )-> 3
2: [⊤]-( {[⊤]⦃C~C,C~C⦄▶⦃⦄×1 % 10+⦃A~C⦄×2 % 5+⦃A~C,B~C⦄×2 % 5+⦃B~C⦄×1 % 10,
          [⦃C~C⦄]⦃⦄▶⦃⦄,
          [⦃C~C,C~C⦄]⦃C~C⦄▶⦃⦄×1 % 5+⦃A~C⦄×4 % 5,
          [⦃C~C,C~C⦄]⦃C~C⦄▶⦃⦄×1 % 2+⦃B~C⦄×1 % 2} )-> 3
3: [⊤]-> $
```

#### Explanation of the output

The automaton for guarded strings naturally has states and transitions, where the transitions are
_guarded_ (i.e., have a test in front) and labelled by _sets of atomic actions_.

The output has the following structure:

 * `n:`---start of the block corresponding to the state of the automaton
 * `[t]-( a )-> k`---transition guarded by test `t` and labelled by a set of atomic actions `a`
   leading to a new state `k`
 * `[t]i▶p`---atomic action with a test `t`, multiset of input bell pairs `i` and distribution over multisets of output Bell pairs `p`


# Reusability Guide

## Development environments

### Preparing development environment

Please, check [Haskell language server documentation][hls] for editor support (optional).

#### Nix

  * install [Nix][nix] package manager
  * enable [Nix flakes][flakes]
  * **to enter environment run** `nix develop` from the artifact's root
  * run `hpack` (no arguments) to generate `.cabal` file

#### Stack

  * install [Stack][stack]
  * install the following extra dependencies: 

     * [Pango][pango] 1.50.6
     * [Cairo][cairo] 1.21.0
     * [Zlib][zlib] 1.3
     * [Glib][glib] 2.72
     * [Ncurses][ncurses] 6.3

     Those can be installed on ubuntu as follows:

     ```bash
     apt-get install libz-dev libtinfo-dev libcairo-dev libpango1.0
     ```

### Building the artifact (recommended to skip)

#### Nix

```bash
cabal build
```

#### Stack

```bash
stack build
```

[nix]: https://nixos.org/download
[flakes]: https://nixos.wiki/wiki/Flakes#Other_Distros.2C_without_Home-Manager
[stack]: https://docs.haskellstack.org/en/stable/
[pango]: https://pango.gnome.org/
[cairo]: https://www.cairographics.org
[zlib]: https://www.zlib.net/
[glib]: https://docs.gtk.org/glib/
[ncurses]: https://invisible-island.net/ncurses/
[docker]: https://docs.docker.com/
[docker-run]: https://docs.docker.com/engine/reference/run/#commands-and-arguments
[haskell]: https://www.haskell.org/
[hls]: https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor
[HSpec]: https://hspec.github.io/
[haddock]: https://haskell-haddock.readthedocs.io/en/latest/
[make]: https://www.gnu.org/software/make/
