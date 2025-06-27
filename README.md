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
---


The artifact is based on a [Haskell][haskell] library `bellkat` plus several examples provided as executables within the same [Haskell][haskell] package.
We provide two build options: [Nix][nix]-based and [Stack][stack]-based, for each we give a [Docker][docker] file that can simplify the setup of the development environment. 
Reproducing the results from the paper can be done in two ways:

  * from a respective development environment ([Nix][nix]- or [Stack][stack]-based) 
  * using an _executable_ [Docker][docker] container providing a [Haskell][haskell] interpreter with `bellkat` already "in scope" (recommended, see "Preparation" section in "Reproducing the results" below)

**Docker note:** please, be aware that `docker run` commands may have to be prefixed with `sudo`,
depending on the docker setup (see details [here][docker-run]).

# Hardware Dependencies

 * RAM: up to 9 GB

# Getting Started

"Getting Started" guide presents only the simplest way to get started with the artifact, namely, using [Docker][docker].
Instructions to create proper development environments, namely using either standard Haskell's [stack][stack] tool or [nix][nix] package manager are given in [Development Environment](#development-environments) section.

## Software requirements

  * [Docker][docker]

## Docker container creation

  * Change to artifact's root
  * Create a [Docker][docker] container by running

    ```bash
    docker build --tag pbkat:latest .
    ```

## Running a simple protocol

To check that everything works as expected we produce a somewhat trivial output for the protocol \S 3(a) using both PBKAT and BellKAT.

_Generally probabilistic "versions" will be preceded by `prob` in the
protocol names_

**PBKAT** generating a convex set of distributions as per example **I** from Section 5.1 (parallel
version), modulo
fractional/decimal notation:

```bash
docker run --rm -i pbkat:latest probP5_1_I_parallel run
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
docker run --rm -i pbkat:latest probP5_1_I_parallel run
# [[],[["A","C"]],[["A","C"],["B","C"]],[["B","C"]]]
```

BellKAT output notation: 

  * `["A", "B"]` denotes a Bell pair $A \sim B$
  * `[[...], [...], ...]` denotes a multiset
  * `[[[...], ...], [[...], ...], ...]` denotes a set of multisets


# Step-by-step Guide

Before diving into the exact steps required to reproduce the results, we explain the inputs to the
tool. 

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

The PBKAT tool can:

  * produce automata capturing guarded strings of sets **TODO**
  * produce the execution traces **TODO**
  * produce the convex set of probability distributions via `run` command (including
    machine-readable form for the next step via `--json` command)
  * analyze the produced convex set of distributions via `probability` command

### Performance measurements (Table 1)

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

The information for the table can be automatically compiled into a `.tex` file using the `collect_stats.py` script.

```bash
docker run --rm -i \
    --mount type=bind,source=$(pwd),target=/opt/pbkat \
    pbkat:latest python collect_stats.py \
        --mode direct --tex --standalone >results.tex
```

Which can then be transformed into `results.pdf` using

```bash
docker run --rm -i \
    --mount type=bind,source=$(pwd),target=/opt/pbkat \
    pbkat:latest pdflatex results.tex
```
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

#### Manual workflow (optional)

The workflow for getting results in Table 1 is the following:

 1. The output, i.e., the convex set of distributions over multisets of Bell pairs represented in the table as a number of generators in column $|O|$, and the performance statistics (columns **Memory** and **Time**) is gathered with

    ```bash
    docker run --rm -i pbkat:latest probPROTO \
        +RTS --machine-readable -t -RTS --json 
        run >PROTO.json 2>PROTO.json.stderr
    ```

     * output goes into `PROTO.json`
     * statistics goes into `PROTO.json.stderr`

 2. Probability $p(\textbf{Goal})$ of successfully generating the required set of Bell
    pairs specified in **Goal** column is computed via

    ```bash
    docker run --rm -i pbkat:latest probPROTO --json run <PROTO.json
    ```

#### Make-automated workflow (optional)

All the experimental results can be generated automatically with [make][make] *(needs to be installed on the host machine)* as follows


```bash
make MODE=docker all-prob
```

**NB** they will be stored in `output/probabilistic/examples` directory.

### Reproducing the execution traces

The general way to produce execution traces for protocol `PROTO` is to execute:

```bash
docker run --rm -i pbkat:latest probPROTO execution-trace
```

E.g., for `PROTO` set to `Pa` (\S 3($a$) in the paper, see [table](#correspondence-table)) the output will be:


```
^0:
  ^⦃⦄: ⦅(1,⦃⦄)×1 % 100+(1,⦃C~C⦄)×9 % 50+(1,⦃C~C,C~C⦄)×81 % 100⦆
1:
  ⦃⦄: ⦅(2,⦃⦄)⦆
  ⦃C~C⦄: ⦅(2,⦃⦄)×1 % 5+(2,⦃A~C⦄)×4 % 5,(2,⦃⦄)×3 % 10+(2,⦃B~C⦄)×7 % 10⦆
  ⦃C~C,C~C⦄: ⦅(2,⦃⦄)×3 % 50+(2,⦃A~C⦄)×6 % 25+(2,⦃A~C,B~C⦄)×14 % 25+(2,⦃B~C⦄)×7 % 50⦆
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

 * Figure 12 in Appendix

    * protocol ($e || f$): `PROTO` is `P5_1_I_parallel`
    * protocol ($e \circ f$): `PROTO` is `P5_1_I_ordered`

# Reusability Guide

## Development environments

### Preparing development environment (recommended to skip)

Please, check [Haskell language server documentation][hls] for editor support (optional).

#### Nix

  * install [Nix][nix] package manager
  * enable [Nix flakes][flakes]
  * **to enter environment run** `nix develop` from the artifact's root
  * run `hpack` (no arguments) to generate `.cabal` file

For convenience, we provide `Dockerfile.nixdev` with the environment already set up:

```bash
docker build --tag bellkat:nixdev --file Dockerfile.nixdev . # build the image
docker run --rm --interactive --tty bellkat:nixdev # to enter the environment
```

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

For convenience, we provide `Dockerfile.stackdev` with the environment already set up:

```bash
docker build --tag bellkat:stackdev --file Dockerfile.stackdev . # build the image
docker run --rm --interactive --tty bellkat:stackdev # to enter the environment
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


## Writing and testing your own protocols

Most of the relevant documentation is present in `BellKAT.Prelude` module. To see it nicely
formatted **(not for docker)**, you can build documentation using [Haddock][haddock]:

 * Stack: `stack haddock`
 * Nix: `cabal haddock`

In both cases, the output should display the path to `index.html` root of the documentation

### Setting up and running the examples

If you want to work with your own protocols or modify existing ones you have multiple options:

  * modify the existing examples

  * create new examples within BellKAT repository

    1. Creating a new file (say `MyExample.hs`) inside `examples/` (e.g., by copying `examples/P3.hs`)

    2. Tell `stack` or `cabal` about the example by adding

       ```yaml
       my-example:
         dependencies: 
           - bellkat
         main: examples/MyExample.hs
       ```

       to `package.yaml`

    3. Run your example with stack, using `stack run my-example` or `cabal run my-example`

    4. (optional) to enable editor support add
       ```yaml
       - path: "examples/MyExample.hs"
         component: "exe:my-example"
       ```
       to `hie.yaml`

    5. Continue editing `examples/MyExample.hs` as you wish.

  * Use `BellKAT` as a library:

     * **Stack:** see the template in `reuse-templates/stack`, then use

       ```bash
       stack build
       stack run
       ```

     * **Nix:** see the template in `reuse-templates/nix`, then use

       ```bash
       nix develop # to enter the shell
       hpack # to generate .cabal file
       cabal build
       cabal run
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
