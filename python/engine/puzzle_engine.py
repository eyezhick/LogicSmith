from typing import Dict, List, Optional, Tuple
import yaml
from pyswip import Prolog
import networkx as nx
import numpy as np
from pathlib import Path

class PuzzleEngine:
    def __init__(self, prolog_path: Optional[str] = None):
        """Initialize the puzzle engine with Prolog integration."""
        self.prolog = Prolog()
        if prolog_path is None:
            prolog_path = Path(__file__).parent.parent.parent / "prolog"
        self._load_prolog_files(prolog_path)
        
    def _load_prolog_files(self, prolog_path: Path):
        """Load all Prolog files from the specified directory."""
        for pl_file in prolog_path.rglob("*.pl"):
            self.prolog.consult(str(pl_file))
            
    def generate(self, 
                puzzle_type: str,
                difficulty: str = "medium",
                theme: Optional[str] = None,
                max_attempts: int = 10) -> Dict:
        """
        Generate a puzzle instance with specified parameters.
        
        Args:
            puzzle_type: Type of puzzle to generate (e.g., "zebra")
            difficulty: Target difficulty level ("easy", "medium", "hard")
            theme: Optional theme for narrative generation
            max_attempts: Maximum number of generation attempts
            
        Returns:
            Dict containing the puzzle instance and metadata
        """
        spec = self._create_spec(puzzle_type, difficulty, theme)
        
        for _ in range(max_attempts):
            try:
                solution, metadata = self._generate_instance(puzzle_type, spec)
                if self._verify_difficulty(metadata, difficulty):
                    return self._package_puzzle(solution, metadata, theme)
            except Exception as e:
                print(f"Generation attempt failed: {e}")
                continue
                
        raise RuntimeError(f"Failed to generate valid puzzle after {max_attempts} attempts")
    
    def _create_spec(self, puzzle_type: str, difficulty: str, theme: Optional[str]) -> str:
        """Create a YAML specification for the puzzle."""
        spec = {
            "type": puzzle_type,
            "difficulty": difficulty,
            "theme": theme,
            "parameters": self._get_puzzle_parameters(puzzle_type, difficulty)
        }
        return yaml.dump(spec)
    
    def _get_puzzle_parameters(self, puzzle_type: str, difficulty: str) -> Dict:
        """Get puzzle-specific parameters based on type and difficulty."""
        # Default parameters for different puzzle types
        params = {
            "zebra": {
                "easy": {"num_houses": 4, "num_clues": 8},
                "medium": {"num_houses": 5, "num_clues": 12},
                "hard": {"num_houses": 5, "num_clues": 15}
            }
            # Add more puzzle types here
        }
        return params.get(puzzle_type, {}).get(difficulty, {})
    
    def _generate_instance(self, puzzle_type: str, spec: str) -> Tuple[Dict, Dict]:
        """Generate a puzzle instance using Prolog."""
        query = f"generate_{puzzle_type}('{spec}', Solution, Metadata)."
        for result in self.prolog.query(query):
            return result["Solution"], result["Metadata"]
        raise RuntimeError(f"Failed to generate {puzzle_type} puzzle")
    
    def _verify_difficulty(self, metadata: Dict, target_difficulty: str) -> bool:
        """Verify if the generated puzzle meets the target difficulty."""
        difficulty_metrics = {
            "easy": {"min_depth": (1, 3), "branching": (1.5, 2.5)},
            "medium": {"min_depth": (3, 5), "branching": (2.5, 3.5)},
            "hard": {"min_depth": (5, 7), "branching": (3.5, 4.5)}
        }
        
        target = difficulty_metrics[target_difficulty]
        return (
            target["min_depth"][0] <= metadata["min_depth"] <= target["min_depth"][1] and
            target["branching"][0] <= metadata["branching_factor"] <= target["branching"][1]
        )
    
    def _package_puzzle(self, solution: Dict, metadata: Dict, theme: Optional[str]) -> Dict:
        """Package the puzzle instance with metadata and theme."""
        return {
            "solution": solution,
            "metadata": metadata,
            "theme": theme,
            "difficulty_metrics": {
                "min_depth": metadata["min_depth"],
                "branching_factor": metadata["branching_factor"],
                "entropy": metadata.get("entropy", 0.0)
            }
        }
    
    def analyze_solution(self, puzzle_type: str, solution: Dict) -> Dict:
        """Analyze a solution's difficulty and characteristics."""
        query = f"analyze_{puzzle_type}_solution('{solution}', Analysis)."
        for result in self.prolog.query(query):
            return result["Analysis"]
        raise RuntimeError(f"Failed to analyze {puzzle_type} solution")
    
    def verify_solution(self, puzzle_type: str, solution: Dict) -> bool:
        """Verify if a solution is valid for the given puzzle type."""
        query = f"verify_{puzzle_type}('{solution}', true)."
        return bool(list(self.prolog.query(query))) 