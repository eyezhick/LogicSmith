from typing import Dict, List, Optional
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
from pathlib import Path
import json

class ResearchVisualizer:
    def __init__(self, results_dir: Optional[str] = None):
        """Initialize the research visualizer."""
        self.results_dir = Path(results_dir) if results_dir else Path("results")
        self.output_dir = self.results_dir / "visualizations"
        self.output_dir.mkdir(exist_ok=True)
        
        # Set style
        plt.style.use("seaborn")
        sns.set_palette("husl")
        
    def create_visualization_package(self, results_file: str, analysis_file: str):
        """
        Create a complete visualization package for research results.
        
        Args:
            results_file: Path to experiment results JSON
            analysis_file: Path to analysis results JSON
        """
        # Load data
        with open(self.results_dir / results_file) as f:
            results = json.load(f)
        with open(self.results_dir / analysis_file) as f:
            analysis = json.load(f)
        
        # Create visualizations
        self._plot_learning_effectiveness(results, analysis)
        self._plot_difficulty_calibration(results, analysis)
        self._plot_hint_effectiveness(results, analysis)
        self._plot_user_satisfaction(results, analysis)
        self._create_summary_dashboard(results, analysis)
        
    def _plot_learning_effectiveness(self, results: Dict, analysis: Dict):
        """Create visualizations for learning effectiveness analysis."""
        # Prepare data
        pre_scores = [p["pre_test_score"] for p in results["participants"]]
        post_scores = [p["post_test_score"] for p in results["participants"]]
        
        # Create figure
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
        
        # Pre-post comparison
        ax1.boxplot([pre_scores, post_scores], labels=["Pre-test", "Post-test"])
        ax1.set_title("Pre-test vs Post-test Scores")
        ax1.set_ylabel("Score")
        
        # Improvement distribution
        improvements = [post - pre for pre, post in zip(pre_scores, post_scores)]
        sns.histplot(improvements, ax=ax2, kde=True)
        ax2.axvline(np.mean(improvements), color="red", linestyle="--", label="Mean")
        ax2.set_title("Distribution of Score Improvements")
        ax2.set_xlabel("Improvement")
        ax2.legend()
        
        plt.tight_layout()
        plt.savefig(self.output_dir / "learning_effectiveness.png")
        plt.close()
        
    def _plot_difficulty_calibration(self, results: Dict, analysis: Dict):
        """Create visualizations for difficulty calibration analysis."""
        # Prepare data
        data = []
        for participant in results["participants"]:
            for i, (rating, level) in enumerate(zip(
                participant["difficulty_ratings"],
                results["difficulty_levels"] * len(results["puzzle_types"])
            )):
                data.append({
                    "puzzle_type": results["puzzle_types"][i // len(results["difficulty_levels"])],
                    "target_difficulty": level,
                    "perceived_difficulty": rating
                })
        
        df = pd.DataFrame(data)
        
        # Create figure
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
        
        # Scatter plot
        sns.scatterplot(
            data=df,
            x="target_difficulty",
            y="perceived_difficulty",
            hue="puzzle_type",
            ax=ax1
        )
        ax1.plot([0, 1], [0, 1], "k--", alpha=0.5)
        ax1.set_title("Target vs Perceived Difficulty")
        
        # Box plot by puzzle type
        sns.boxplot(
            data=df,
            x="puzzle_type",
            y="perceived_difficulty",
            ax=ax2
        )
        ax2.set_title("Perceived Difficulty by Puzzle Type")
        ax2.set_xticklabels(ax2.get_xticklabels(), rotation=45)
        
        plt.tight_layout()
        plt.savefig(self.output_dir / "difficulty_calibration.png")
        plt.close()
        
    def _plot_hint_effectiveness(self, results: Dict, analysis: Dict):
        """Create visualizations for hint effectiveness analysis."""
        # Prepare data
        data = []
        for participant in results["participants"]:
            for i, (hints, time) in enumerate(zip(
                participant["hint_usage"],
                participant["solving_times"]
            )):
                data.append({
                    "puzzle_type": results["puzzle_types"][i // len(results["difficulty_levels"])],
                    "difficulty": results["difficulty_levels"][i % len(results["difficulty_levels"])],
                    "hints_used": hints,
                    "solving_time": time
                })
        
        df = pd.DataFrame(data)
        
        # Create figure
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
        
        # Hint usage by difficulty
        sns.boxplot(
            data=df,
            x="difficulty",
            y="hints_used",
            ax=ax1
        )
        ax1.set_title("Hint Usage by Difficulty Level")
        
        # Solving time vs hints
        sns.scatterplot(
            data=df,
            x="hints_used",
            y="solving_time",
            hue="puzzle_type",
            ax=ax2
        )
        ax2.set_title("Solving Time vs Hint Usage")
        
        plt.tight_layout()
        plt.savefig(self.output_dir / "hint_effectiveness.png")
        plt.close()
        
    def _plot_user_satisfaction(self, results: Dict, analysis: Dict):
        """Create visualizations for user satisfaction analysis."""
        # Prepare data
        data = []
        for participant in results["participants"]:
            for i, rating in enumerate(participant["satisfaction_ratings"]):
                data.append({
                    "puzzle_type": results["puzzle_types"][i // len(results["difficulty_levels"])],
                    "difficulty": results["difficulty_levels"][i % len(results["difficulty_levels"])],
                    "satisfaction": rating
                })
        
        df = pd.DataFrame(data)
        
        # Create figure
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
        
        # Satisfaction by puzzle type
        sns.boxplot(
            data=df,
            x="puzzle_type",
            y="satisfaction",
            ax=ax1
        )
        ax1.set_title("Satisfaction by Puzzle Type")
        ax1.set_xticklabels(ax1.get_xticklabels(), rotation=45)
        
        # Satisfaction by difficulty
        sns.boxplot(
            data=df,
            x="difficulty",
            y="satisfaction",
            ax=ax2
        )
        ax2.set_title("Satisfaction by Difficulty Level")
        
        plt.tight_layout()
        plt.savefig(self.output_dir / "user_satisfaction.png")
        plt.close()
        
    def _create_summary_dashboard(self, results: Dict, analysis: Dict):
        """Create a summary dashboard of key metrics."""
        # Create figure
        fig = plt.figure(figsize=(15, 10))
        gs = fig.add_gridspec(3, 2)
        
        # Learning effectiveness
        ax1 = fig.add_subplot(gs[0, 0])
        pre_scores = [p["pre_test_score"] for p in results["participants"]]
        post_scores = [p["post_test_score"] for p in results["participants"]]
        ax1.boxplot([pre_scores, post_scores], labels=["Pre-test", "Post-test"])
        ax1.set_title("Learning Effectiveness")
        
        # Difficulty calibration
        ax2 = fig.add_subplot(gs[0, 1])
        data = []
        for participant in results["participants"]:
            for i, (rating, level) in enumerate(zip(
                participant["difficulty_ratings"],
                results["difficulty_levels"] * len(results["puzzle_types"])
            )):
                data.append({
                    "target": level,
                    "perceived": rating
                })
        df = pd.DataFrame(data)
        sns.scatterplot(data=df, x="target", y="perceived", ax=ax2)
        ax2.plot([0, 1], [0, 1], "k--", alpha=0.5)
        ax2.set_title("Difficulty Calibration")
        
        # Hint effectiveness
        ax3 = fig.add_subplot(gs[1, 0])
        data = []
        for participant in results["participants"]:
            for i, (hints, time) in enumerate(zip(
                participant["hint_usage"],
                participant["solving_times"]
            )):
                data.append({
                    "hints": hints,
                    "time": time
                })
        df = pd.DataFrame(data)
        sns.scatterplot(data=df, x="hints", y="time", ax=ax3)
        ax3.set_title("Hint Effectiveness")
        
        # User satisfaction
        ax4 = fig.add_subplot(gs[1, 1])
        data = []
        for participant in results["participants"]:
            for i, rating in enumerate(participant["satisfaction_ratings"]):
                data.append({
                    "puzzle_type": results["puzzle_types"][i // len(results["difficulty_levels"])],
                    "satisfaction": rating
                })
        df = pd.DataFrame(data)
        sns.boxplot(data=df, x="puzzle_type", y="satisfaction", ax=ax4)
        ax4.set_title("User Satisfaction")
        ax4.set_xticklabels(ax4.get_xticklabels(), rotation=45)
        
        # Key metrics
        ax5 = fig.add_subplot(gs[2, :])
        metrics = [
            f"Learning Improvement: {analysis['learning_effectiveness']['mean_improvement']:.2f}",
            f"Difficulty RMSE: {analysis['difficulty_calibration']['rmse']:.2f}",
            f"Avg Hints per Puzzle: {analysis['hint_effectiveness']['avg_hints_per_puzzle']:.1f}",
            f"Overall Satisfaction: {analysis['user_satisfaction']['overall_satisfaction']:.2f}"
        ]
        ax5.text(0.1, 0.5, "\n".join(metrics), fontsize=12)
        ax5.axis("off")
        
        plt.tight_layout()
        plt.savefig(self.output_dir / "summary_dashboard.png")
        plt.close() 