"""Entry point: ``python -I -B .../gambitr3d_worker/__main__.py [options]``.

The interpreter runs in isolated mode (-I), so neither the current
directory nor PYTHON* variables can inject modules. The package's parent
directory is added explicitly so the worker is importable when run as a
script.
"""

import os
import sys

if __package__ in (None, ""):
    sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from gambitr3d_worker.protocol import main  # noqa: E402

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
