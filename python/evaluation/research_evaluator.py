from typing import Dict, List, Optional, Tuple
import numpy as np
from scipy import stats
import pandas as pd
from dataclasses import dataclass
from datetime import datetime
import json
from pathlib import Path

@dataclass
class ParticipantData:
    id: str
    age: int
    experience_level: str
    pre_test_score: float
    post_test_score: float
    solving_times: List[float]
    hint_usage: List[int]
    difficulty_ratings: List[float]
    satisfaction_ratings: List[float]

@dataclass
class ExperimentResults:
    participants: List[ParticipantData]
    puzzle_types: List[str]
    difficulty_levels: List[str]
    start_time: datetime
    end_time: datetime
    metadata: Dict

class ResearchEvaluator:
    def __init__(self, results_dir: Optional[str] = None):
        """Initialize the research evaluator."""
        self.results_dir = Path(results_dir) if results_dir else Path("results")
        self.results_dir.mkdir(exist_ok=True)
        
    def run_experiment(self,
                      puzzle_types: List[str],
                      difficulty_levels: List[str],
                      num_participants: int,
                      metadata: Optional[Dict] = None) -> ExperimentResults:
        """
        Run a controlled experiment with the specified parameters.
        
        Args:
            puzzle_types: Types of puzzles to include
            difficulty_levels: Difficulty levels to test
            num_participants: Number of participants
            metadata: Additional experiment metadata
            
        Returns:
            ExperimentResults containing all collected data
        """
        start_time = datetime.now()
        participants = []
        
        for i in range(num_participants):
            participant = self._run_participant_session(
                participant_id=f"P{i+1:03d}",
                puzzle_types=puzzle_types,
                difficulty_levels=difficulty_levels
            )
            participants.append(participant)
        
        end_time = datetime.now()
        
        results = ExperimentResults(
            participants=participants,
            puzzle_types=puzzle_types,
            difficulty_levels=difficulty_levels,
            start_time=start_time,
            end_time=end_time,
            metadata=metadata or {}
        )
        
        self._save_results(results)
        return results
    
    def analyze_results(self, results: ExperimentResults) -> Dict:
        """
        Perform comprehensive analysis of experiment results.
        
        Args:
            results: ExperimentResults to analyze
            
        Returns:
            Dict containing various analysis metrics
        """
        analysis = {
            "learning_effectiveness": self._analyze_learning_effectiveness(results),
            "difficulty_calibration": self._analyze_difficulty_calibration(results),
            "hint_effectiveness": self._analyze_hint_effectiveness(results),
            "user_satisfaction": self._analyze_user_satisfaction(results),
            "statistical_tests": self._perform_statistical_tests(results)
        }
        
        self._save_analysis(analysis, results)
        return analysis
    
    def _run_participant_session(self,
                               participant_id: str,
                               puzzle_types: List[str],
                               difficulty_levels: List[str]) -> ParticipantData:
        """Run a single participant's experimental session."""
        # Collect demographic data
        age = self._collect_age()
        experience_level = self._collect_experience_level()
        
        # Administer pre-test
        pre_test_score = self._administer_pre_test()
        
        # Run puzzle sessions
        solving_times = []
        hint_usage = []
        difficulty_ratings = []
        satisfaction_ratings = []
        
        for puzzle_type in puzzle_types:
            for difficulty in difficulty_levels:
                # Generate and present puzzle
                puzzle = self._generate_puzzle(puzzle_type, difficulty)
                
                # Record solving process
                start_time = datetime.now()
                hints_used = self._present_puzzle(puzzle)
                solving_time = (datetime.now() - start_time).total_seconds()
                
                # Collect ratings
                difficulty_rating = self._collect_difficulty_rating()
                satisfaction_rating = self._collect_satisfaction_rating()
                
                # Store results
                solving_times.append(solving_time)
                hint_usage.append(hints_used)
                difficulty_ratings.append(difficulty_rating)
                satisfaction_ratings.append(satisfaction_rating)
        
        # Administer post-test
        post_test_score = self._administer_post_test()
        
        return ParticipantData(
            id=participant_id,
            age=age,
            experience_level=experience_level,
            pre_test_score=pre_test_score,
            post_test_score=post_test_score,
            solving_times=solving_times,
            hint_usage=hint_usage,
            difficulty_ratings=difficulty_ratings,
            satisfaction_ratings=satisfaction_ratings
        )
    
    def _analyze_learning_effectiveness(self, results: ExperimentResults) -> Dict:
        """Analyze the effectiveness of puzzles for learning."""
        pre_scores = [p.pre_test_score for p in results.participants]
        post_scores = [p.post_test_score for p in results.participants]
        
        # Calculate improvement
        improvements = [post - pre for pre, post in zip(pre_scores, post_scores)]
        
        return {
            "mean_improvement": np.mean(improvements),
            "std_improvement": np.std(improvements),
            "effect_size": self._calculate_cohens_d(pre_scores, post_scores),
            "significant_improvement": self._perform_ttest(pre_scores, post_scores)
        }
    
    def _analyze_difficulty_calibration(self, results: ExperimentResults) -> Dict:
        """Analyze the accuracy of difficulty calibration."""
        calibration_data = []
        
        for participant in results.participants:
            for i, (rating, level) in enumerate(zip(
                participant.difficulty_ratings,
                results.difficulty_levels * len(results.puzzle_types)
            )):
                calibration_data.append({
                    "participant": participant.id,
                    "puzzle_type": results.puzzle_types[i // len(results.difficulty_levels)],
                    "target_difficulty": level,
                    "perceived_difficulty": rating
                })
        
        df = pd.DataFrame(calibration_data)
        
        return {
            "rmse": self._calculate_rmse(df["target_difficulty"], df["perceived_difficulty"]),
            "correlation": df["target_difficulty"].corr(df["perceived_difficulty"]),
            "by_puzzle_type": df.groupby("puzzle_type").apply(
                lambda x: x["target_difficulty"].corr(x["perceived_difficulty"])
            ).to_dict()
        }
    
    def _analyze_hint_effectiveness(self, results: ExperimentResults) -> Dict:
        """Analyze the effectiveness of the hint system."""
        hint_data = []
        
        for participant in results.participants:
            for i, hints in enumerate(participant.hint_usage):
                hint_data.append({
                    "participant": participant.id,
                    "puzzle_type": results.puzzle_types[i // len(results.difficulty_levels)],
                    "difficulty": results.difficulty_levels[i % len(results.difficulty_levels)],
                    "hints_used": hints,
                    "solving_time": participant.solving_times[i]
                })
        
        df = pd.DataFrame(hint_data)
        
        return {
            "avg_hints_per_puzzle": df["hints_used"].mean(),
            "hint_usage_by_difficulty": df.groupby("difficulty")["hints_used"].mean().to_dict(),
            "hint_impact_on_time": df.groupby("hints_used")["solving_time"].mean().to_dict()
        }
    
    def _analyze_user_satisfaction(self, results: ExperimentResults) -> Dict:
        """Analyze user satisfaction metrics."""
        satisfaction_data = []
        
        for participant in results.participants:
            for i, rating in enumerate(participant.satisfaction_ratings):
                satisfaction_data.append({
                    "participant": participant.id,
                    "puzzle_type": results.puzzle_types[i // len(results.difficulty_levels)],
                    "difficulty": results.difficulty_levels[i % len(results.difficulty_levels)],
                    "satisfaction": rating
                })
        
        df = pd.DataFrame(satisfaction_data)
        
        return {
            "overall_satisfaction": df["satisfaction"].mean(),
            "satisfaction_by_type": df.groupby("puzzle_type")["satisfaction"].mean().to_dict(),
            "satisfaction_by_difficulty": df.groupby("difficulty")["satisfaction"].mean().to_dict()
        }
    
    def _perform_statistical_tests(self, results: ExperimentResults) -> Dict:
        """Perform various statistical tests on the results."""
        return {
            "learning_improvement": self._perform_ttest(
                [p.pre_test_score for p in results.participants],
                [p.post_test_score for p in results.participants]
            ),
            "difficulty_correlation": stats.pearsonr(
                [r for p in results.participants for r in p.difficulty_ratings],
                [d for _ in results.participants for d in results.difficulty_levels * len(results.puzzle_types)]
            ),
            "hint_usage_anova": self._perform_anova(
                [p.hint_usage for p in results.participants],
                results.difficulty_levels * len(results.puzzle_types)
            )
        }
    
    def _calculate_cohens_d(self, pre: List[float], post: List[float]) -> float:
        """Calculate Cohen's d effect size."""
        n1, n2 = len(pre), len(post)
        var1, var2 = np.var(pre, ddof=1), np.var(post, ddof=1)
        
        # Calculate pooled standard deviation
        pooled_std = np.sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
        
        # Calculate Cohen's d
        return (np.mean(post) - np.mean(pre)) / pooled_std
    
    def _calculate_rmse(self, predicted: List[float], actual: List[float]) -> float:
        """Calculate Root Mean Square Error."""
        return np.sqrt(np.mean((np.array(predicted) - np.array(actual)) ** 2))
    
    def _perform_ttest(self, group1: List[float], group2: List[float]) -> Dict:
        """Perform a paired t-test."""
        t_stat, p_val = stats.ttest_rel(group1, group2)
        return {
            "t_statistic": t_stat,
            "p_value": p_val,
            "significant": p_val < 0.05
        }
    
    def _perform_anova(self, groups: List[List[float]], labels: List[str]) -> Dict:
        """Perform one-way ANOVA."""
        f_stat, p_val = stats.f_oneway(*groups)
        return {
            "f_statistic": f_stat,
            "p_value": p_val,
            "significant": p_val < 0.05
        }
    
    def _save_results(self, results: ExperimentResults):
        """Save experiment results to disk."""
        timestamp = results.start_time.strftime("%Y%m%d_%H%M%S")
        results_file = self.results_dir / f"experiment_{timestamp}.json"
        
        data = {
            "participants": [
                {
                    "id": p.id,
                    "age": p.age,
                    "experience_level": p.experience_level,
                    "pre_test_score": p.pre_test_score,
                    "post_test_score": p.post_test_score,
                    "solving_times": p.solving_times,
                    "hint_usage": p.hint_usage,
                    "difficulty_ratings": p.difficulty_ratings,
                    "satisfaction_ratings": p.satisfaction_ratings
                }
                for p in results.participants
            ],
            "puzzle_types": results.puzzle_types,
            "difficulty_levels": results.difficulty_levels,
            "start_time": results.start_time.isoformat(),
            "end_time": results.end_time.isoformat(),
            "metadata": results.metadata
        }
        
        with open(results_file, "w") as f:
            json.dump(data, f, indent=2)
    
    def _save_analysis(self, analysis: Dict, results: ExperimentResults):
        """Save analysis results to disk."""
        timestamp = results.start_time.strftime("%Y%m%d_%H%M%S")
        analysis_file = self.results_dir / f"analysis_{timestamp}.json"
        
        with open(analysis_file, "w") as f:
            json.dump(analysis, f, indent=2)
    
    # Placeholder methods for data collection
    def _collect_age(self) -> int:
        """Collect participant's age."""
        return 25  # Placeholder
    
    def _collect_experience_level(self) -> str:
        """Collect participant's experience level."""
        return "intermediate"  # Placeholder
    
    def _administer_pre_test(self) -> float:
        """Administer pre-test and return score."""
        return 0.7  # Placeholder
    
    def _administer_post_test(self) -> float:
        """Administer post-test and return score."""
        return 0.85  # Placeholder
    
    def _generate_puzzle(self, puzzle_type: str, difficulty: str) -> Dict:
        """Generate a puzzle instance."""
        return {}  # Placeholder
    
    def _present_puzzle(self, puzzle: Dict) -> int:
        """Present puzzle to participant and return number of hints used."""
        return 2  # Placeholder
    
    def _collect_difficulty_rating(self) -> float:
        """Collect participant's difficulty rating."""
        return 0.6  # Placeholder
    
    def _collect_satisfaction_rating(self) -> float:
        """Collect participant's satisfaction rating."""
        return 0.8  # Placeholder 