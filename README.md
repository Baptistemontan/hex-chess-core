Hexagonal grid coordinate system ressource: https://www.redblobgames.com/grids/hexagons/#coordinates
Use the `Axial` coordinates (flat)

Hex chess ressource: https://en.wikipedia.org/wiki/Hexagonal_chess
Gliński's variant

The Board history is considered the source of truth, therefore when serializing the board only the history is serialized, and the board is then reconstructed from here, returning an error if any of those moves are illegals.
