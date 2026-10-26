"""Optional tensor worker for gambitR3D.

The worker batches neural-network inference and training for the R package.
It never implements the game rules and never produces a board state: R
encodes positions and their legal candidate actions, the worker returns
scores, and R validates and maps them back to action ids.
"""

__version__ = "0.1.0"
PROTOCOL_VERSION = "1.0"
PROTOCOL_MAJOR = 1
