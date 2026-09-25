"""Demographic labels shared by models that take gender/race/ethnicity codes.

Labels follow the training-time vocabulary ("F"/"M", "white"/"black"/...,
"nonhispanic"/"hispanic"); each model maps them to its own integer codes.
Sources are merged in precedence order: explicit request fields, then the
stored record's columns, then the demographic preamble in the HPI text.
"""
from __future__ import annotations

import re
from dataclasses import dataclass

GENDERS = ("F", "M")
RACES = ("white", "black", "asian", "native", "other")
ETHNICITIES = ("nonhispanic", "hispanic")

# Matches the demographic preamble kept by the original preprocessing,
# e.g. "24 year-old nonhispanic white male". Bare word search is avoided
# because race terms collide with clinical text ("white blood cells").
DEMOGRAPHIC_PHRASE = re.compile(
    r"\b(nonhispanic|hispanic)\s+(white|black|asian|native|other)\b"
)
FEMALE_PATTERN = re.compile(r"\bfemale\b")
MALE_PATTERN = re.compile(r"\bmale\b")


@dataclass(frozen=True)
class Demographics:
    gender: str | None = None
    race: str | None = None
    ethnicity: str | None = None

    def merged_with(self, fallback: "Demographics") -> "Demographics":
        return Demographics(
            gender=self.gender or fallback.gender,
            race=self.race or fallback.race,
            ethnicity=self.ethnicity or fallback.ethnicity,
        )


def _normalize(value: object, allowed: tuple[str, ...]) -> str | None:
    if not isinstance(value, str) or not value.strip():
        return None
    cleaned = value.strip()
    for label in allowed:
        if cleaned.lower() == label.lower():
            return label
    # Unknown labels (e.g. "hawaiian") are passed through so each model can
    # apply its own unmapped-value handling.
    return cleaned.lower()


def from_fields(gender: object, race: object, ethnicity: object) -> Demographics:
    return Demographics(
        gender=_normalize(gender, GENDERS),
        race=_normalize(race, RACES),
        ethnicity=_normalize(ethnicity, ETHNICITIES),
    )


def from_text(text: str) -> Demographics:
    lowered = text.lower()
    gender = None
    if FEMALE_PATTERN.search(lowered):
        gender = "F"
    elif MALE_PATTERN.search(lowered):
        gender = "M"

    race = None
    ethnicity = None
    match = DEMOGRAPHIC_PHRASE.search(lowered)
    if match:
        ethnicity = match.group(1)
        race = match.group(2)
    return Demographics(gender=gender, race=race, ethnicity=ethnicity)
