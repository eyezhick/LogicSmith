from typing import Dict, List, Tuple
import networkx as nx
import numpy as np
from scipy.stats import entropy
from dataclasses import dataclass
from enum import Enum

class ConstraintType(Enum):
    UNARY = 1
    BINARY = 2
    N_ARY = 3
    IMPLICATION = 4
    EQUIVALENCE = 5

@dataclass
class ConstraintMetrics:
    density: float
    arity_distribution: Dict[int, float]
    type_distribution: Dict[ConstraintType, float]
    connectivity: float

@dataclass
class SearchMetrics:
    min_depth: int
    avg_branching: float
    entropy: float
    solution_count: int
    search_space_size: int

@dataclass
class DifficultyProfile:
    syntactic_complexity: float
    semantic_complexity: float
    search_complexity: float
    cognitive_load: float
    overall_difficulty: float

class DifficultyAnalyzer:
    def __init__(self):
        """Initialize the difficulty analyzer with default parameters."""
        self.cognitive_load_weights = {
            'working_memory': 0.3,
            'inference_steps': 0.3,
            'constraint_types': 0.2,
            'solution_space': 0.2
        }
        
    def analyze_puzzle(self, 
                      constraint_graph: nx.DiGraph,
                      solution_space: List[Dict],
                      search_tree: nx.DiGraph) -> DifficultyProfile:
        """
        Perform comprehensive difficulty analysis of a puzzle instance.
        
        Args:
            constraint_graph: Graph representing puzzle constraints
            solution_space: List of valid solutions
            search_tree: Tree representing the search process
            
        Returns:
            DifficultyProfile containing various complexity metrics
        """
        # Analyze different aspects of difficulty
        syntactic = self._analyze_syntactic_complexity(constraint_graph)
        semantic = self._analyze_semantic_complexity(constraint_graph)
        search = self._analyze_search_complexity(search_tree, solution_space)
        cognitive = self._estimate_cognitive_load(syntactic, semantic, search)
        
        # Calculate overall difficulty
        overall = self._calculate_overall_difficulty(syntactic, semantic, search, cognitive)
        
        return DifficultyProfile(
            syntactic_complexity=syntactic,
            semantic_complexity=semantic,
            search_complexity=search,
            cognitive_load=cognitive,
            overall_difficulty=overall
        )
    
    def _analyze_syntactic_complexity(self, graph: nx.DiGraph) -> float:
        """Analyze the syntactic complexity of the constraint system."""
        # Calculate constraint density
        n_nodes = graph.number_of_nodes()
        n_edges = graph.number_of_edges()
        density = n_edges / (n_nodes * (n_nodes - 1)) if n_nodes > 1 else 0
        
        # Analyze constraint arity distribution
        arity_dist = self._calculate_arity_distribution(graph)
        
        # Calculate syntactic complexity score
        complexity = (
            0.4 * density +
            0.3 * np.mean(list(arity_dist.values())) +
            0.3 * len(arity_dist) / n_nodes
        )
        
        return min(1.0, complexity)
    
    def _analyze_semantic_complexity(self, graph: nx.DiGraph) -> float:
        """Analyze the semantic complexity of the constraint system."""
        # Analyze constraint types
        type_dist = self._analyze_constraint_types(graph)
        
        # Calculate semantic complexity score
        complexity = sum(
            weight * count
            for weight, count in type_dist.items()
        ) / graph.number_of_edges()
        
        return min(1.0, complexity)
    
    def _analyze_search_complexity(self, 
                                 search_tree: nx.DiGraph,
                                 solution_space: List[Dict]) -> float:
        """Analyze the complexity of the search space."""
        # Calculate search metrics
        metrics = self._calculate_search_metrics(search_tree, solution_space)
        
        # Normalize metrics
        normalized_depth = metrics.min_depth / 10  # Assuming max depth of 10
        normalized_branching = metrics.avg_branching / 5  # Assuming max branching of 5
        normalized_entropy = metrics.entropy / np.log(metrics.solution_count + 1)
        
        # Calculate search complexity score
        complexity = (
            0.4 * normalized_depth +
            0.3 * normalized_branching +
            0.3 * normalized_entropy
        )
        
        return min(1.0, complexity)
    
    def _estimate_cognitive_load(self,
                               syntactic: float,
                               semantic: float,
                               search: float) -> float:
        """Estimate the cognitive load of solving the puzzle."""
        # Calculate weighted cognitive load
        load = (
            self.cognitive_load_weights['working_memory'] * syntactic +
            self.cognitive_load_weights['inference_steps'] * search +
            self.cognitive_load_weights['constraint_types'] * semantic +
            self.cognitive_load_weights['solution_space'] * search
        )
        
        return min(1.0, load)
    
    def _calculate_overall_difficulty(self,
                                   syntactic: float,
                                   semantic: float,
                                   search: float,
                                   cognitive: float) -> float:
        """Calculate the overall difficulty score."""
        return (
            0.25 * syntactic +
            0.25 * semantic +
            0.25 * search +
            0.25 * cognitive
        )
    
    def _calculate_arity_distribution(self, graph: nx.DiGraph) -> Dict[int, float]:
        """Calculate the distribution of constraint arities."""
        arities = {}
        for node in graph.nodes():
            arity = graph.degree(node)
            arities[arity] = arities.get(arity, 0) + 1
        
        total = sum(arities.values())
        return {k: v/total for k, v in arities.items()}
    
    def _analyze_constraint_types(self, graph: nx.DiGraph) -> Dict[float, int]:
        """Analyze the types of constraints present in the graph."""
        type_counts = {t: 0 for t in ConstraintType}
        
        for edge in graph.edges():
            # Analyze edge properties to determine constraint type
            if graph.edges[edge].get('unary', False):
                type_counts[ConstraintType.UNARY] += 1
            elif graph.edges[edge].get('implication', False):
                type_counts[ConstraintType.IMPLICATION] += 1
            elif graph.edges[edge].get('equivalence', False):
                type_counts[ConstraintType.EQUIVALENCE] += 1
            elif len(graph.edges[edge].get('variables', [])) > 2:
                type_counts[ConstraintType.N_ARY] += 1
            else:
                type_counts[ConstraintType.BINARY] += 1
        
        # Assign weights to different constraint types
        weights = {
            ConstraintType.UNARY: 0.5,
            ConstraintType.BINARY: 1.0,
            ConstraintType.N_ARY: 1.5,
            ConstraintType.IMPLICATION: 1.2,
            ConstraintType.EQUIVALENCE: 1.3
        }
        
        return {weights[t]: count for t, count in type_counts.items()}
    
    def _calculate_search_metrics(self,
                                search_tree: nx.DiGraph,
                                solution_space: List[Dict]) -> SearchMetrics:
        """Calculate metrics about the search space."""
        # Calculate minimum depth
        min_depth = min(
            len(nx.shortest_path(search_tree, source=root, target=leaf))
            for root in [n for n, d in search_tree.in_degree() if d == 0]
            for leaf in [n for n, d in search_tree.out_degree() if d == 0]
        )
        
        # Calculate average branching factor
        branching_factors = [
            search_tree.out_degree(node)
            for node in search_tree.nodes()
            if search_tree.out_degree(node) > 0
        ]
        avg_branching = np.mean(branching_factors) if branching_factors else 0
        
        # Calculate solution space entropy
        solution_count = len(solution_space)
        search_space_size = search_tree.number_of_nodes()
        entropy_val = entropy([1/solution_count] * solution_count) if solution_count > 0 else 0
        
        return SearchMetrics(
            min_depth=min_depth,
            avg_branching=avg_branching,
            entropy=entropy_val,
            solution_count=solution_count,
            search_space_size=search_space_size
        ) 