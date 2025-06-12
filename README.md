# LogicSmith

A hybrid symbolic–neural framework for procedural generation and pedagogical presentation of logic puzzles.

## Overview

LogicSmith is an open-source research platform that combines symbolic reasoning (Prolog), probabilistic difficulty estimation (Python), and large-language-model narrative generation to automatically synthesize, verify, and adapt logic puzzle instances.

### Key Features

- **Symbolic Constraint Solving**: Deterministic search in Prolog guarantees soundness and completeness
- **Statistical Heuristics**: Python-based analysis of solution graphs for difficulty estimation
- **Neural Language Generation**: LLM-powered conversion of constraints into themed storylines
- **Interactive Interface**: Web-based UI for puzzle presentation and hint delivery

## Project Structure

```
logicsmith/
├── prolog/                 # Prolog constraint definitions
│   ├── generators/        # Puzzle family generators
│   ├── dsl_macros.pl     # Domain-specific language macros
│   └── utils/            # Helper predicates
├── python/               # Python orchestration layer
│   ├── engine/          # Core generation engine
│   ├── analysis/        # Difficulty analysis
│   └── api/             # REST API endpoints
├── frontend/            # Streamlit web interface
├── tests/               # Test suite
└── examples/            # Example puzzle specifications
```

## Prerequisites

- Python 3.11+
- SWI-Prolog 8.4+
- OpenAI API key (for narrative generation)

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/logicsmith.git
cd logicsmith
```

2. Create and activate a virtual environment:
```bash
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

3. Install dependencies:
```bash
pip install -r requirements.txt
```

4. Set up environment variables:
```bash
cp .env.example .env
# Edit .env with your OpenAI API key
```

## Usage

### Generating Puzzles

```python
from logicsmith.engine import PuzzleEngine

engine = PuzzleEngine()
puzzle = engine.generate(
    puzzle_type="zebra",
    difficulty="medium",
    theme="detective"
)
```

### Running the Web Interface

```bash
streamlit run frontend/app.py
```

## Development

### Running Tests

```bash
pytest tests/
```

### Adding New Puzzle Types

1. Create a new generator in `prolog/generators/`
2. Define constraints using the DSL macros
3. Add difficulty metrics in `python/analysis/`
4. Create narrative templates in `python/narrator/`

## Contributing

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use LogicSmith in your research, please cite:

```bibtex
@software{logicsmith2024,
  author = {Your Name},
  title = {LogicSmith: A Hybrid Symbolic–Neural Framework for Logic Puzzles},
  year = {2024},
  url = {https://github.com/yourusername/logicsmith}
}
```
