"""Paper-facing acronyms for protocol schemes.

Keep these values aligned with the evaluation-scheme macros in the paper's
``macros.tex``.  Internal protocol names and output filenames deliberately
remain unchanged so existing experiment data can still be reused.
"""

SCHEME_ACRONYMS = {
    "doubling": "DBL",
    "sequential": "SEQ",
    "left-to-right": "L2R",
    "right-to-left": "R2L",
    "swap-asap": "ASAP",
    "swap": "SW",
    "dist-swap": "D-S",
    "swap-dist": "S-D",
}

SCHEME_TEXTSC_CONTENT = {
    "doubling": "Dbl",
    "sequential": "Seq",
    "left-to-right": "l2r",
    "right-to-left": "r2l",
    "swap-asap": "asap",
    "swap": "Sw",
    "dist-swap": "d-s",
    "swap-dist": "s-d",
}


def scheme_acronym(scheme: str) -> str:
    """Return a plain-text fallback for the paper acronym."""
    return SCHEME_ACRONYMS[scheme]


def scheme_latex_acronym(scheme: str) -> str:
    """Return the exact small-caps expression used by the paper macros."""
    return rf"\textsc{{{SCHEME_TEXTSC_CONTENT[scheme]}}}"


def latex_text_enabled() -> bool:
    """Report whether Matplotlib is currently delegating text to LaTeX."""
    try:
        from matplotlib import rcParams
    except ImportError:
        return False
    return bool(rcParams["text.usetex"])


def scheme_plot_label(scheme: str) -> str:
    """Return a text-context label for the active Matplotlib renderer."""
    if latex_text_enabled():
        return scheme_latex_acronym(scheme)
    return scheme_acronym(scheme)


def scheme_math_acronym(scheme: str) -> str:
    """Return a math-context label for the active Matplotlib renderer."""
    if latex_text_enabled():
        return rf"\text{{{scheme_latex_acronym(scheme)}}}"
    acronym = scheme_acronym(scheme).replace("-", r"\!-\!")
    return rf"\mathrm{{{acronym}}}"
