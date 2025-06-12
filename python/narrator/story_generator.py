from typing import Dict, List, Optional
import json
from pathlib import Path
import openai
from dotenv import load_dotenv
import os

class StoryGenerator:
    def __init__(self, api_key: Optional[str] = None):
        """Initialize the story generator with OpenAI API integration."""
        load_dotenv()
        self.api_key = api_key or os.getenv("OPENAI_API_KEY")
        if not self.api_key:
            raise ValueError("OpenAI API key not found")
        openai.api_key = self.api_key
        
        self.template_dir = Path(__file__).parent / "templates"
        self._load_templates()
        
    def _load_templates(self):
        """Load narrative templates from the templates directory."""
        self.templates = {}
        for template_file in self.template_dir.glob("*.json"):
            with open(template_file) as f:
                self.templates[template_file.stem] = json.load(f)
                
    def generate_story(self, 
                      puzzle_type: str,
                      solution: Dict,
                      theme: Optional[str] = None,
                      difficulty: str = "medium") -> Dict:
        """
        Generate a themed story and hints for a puzzle instance.
        
        Args:
            puzzle_type: Type of puzzle (e.g., "zebra")
            solution: The puzzle solution
            theme: Optional theme for the narrative
            difficulty: Difficulty level for hint generation
            
        Returns:
            Dict containing the story, hints, and explanation
        """
        template = self.templates.get(puzzle_type, self.templates["default"])
        
        # Generate the main story
        story = self._generate_main_story(template, solution, theme)
        
        # Generate progressive hints
        hints = self._generate_hints(template, solution, difficulty)
        
        # Generate solution explanation
        explanation = self._generate_explanation(template, solution)
        
        return {
            "story": story,
            "hints": hints,
            "explanation": explanation,
            "theme": theme,
            "difficulty": difficulty
        }
    
    def _generate_main_story(self, template: Dict, solution: Dict, theme: Optional[str]) -> str:
        """Generate the main puzzle story using the LLM."""
        prompt = self._create_story_prompt(template, solution, theme)
        
        response = openai.ChatCompletion.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": template["system_prompt"]},
                {"role": "user", "content": prompt}
            ],
            temperature=0.7,
            max_tokens=1000
        )
        
        return response.choices[0].message.content
    
    def _generate_hints(self, template: Dict, solution: Dict, difficulty: str) -> List[str]:
        """Generate progressive hints based on difficulty level."""
        num_hints = {
            "easy": 3,
            "medium": 4,
            "hard": 5
        }[difficulty]
        
        hints = []
        for i in range(num_hints):
            prompt = self._create_hint_prompt(template, solution, i, num_hints)
            
            response = openai.ChatCompletion.create(
                model="gpt-4",
                messages=[
                    {"role": "system", "content": template["hint_system_prompt"]},
                    {"role": "user", "content": prompt}
                ],
                temperature=0.5,
                max_tokens=200
            )
            
            hints.append(response.choices[0].message.content)
            
        return hints
    
    def _generate_explanation(self, template: Dict, solution: Dict) -> str:
        """Generate a detailed solution explanation."""
        prompt = self._create_explanation_prompt(template, solution)
        
        response = openai.ChatCompletion.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": template["explanation_system_prompt"]},
                {"role": "user", "content": prompt}
            ],
            temperature=0.3,
            max_tokens=500
        )
        
        return response.choices[0].message.content
    
    def _create_story_prompt(self, template: Dict, solution: Dict, theme: Optional[str]) -> str:
        """Create the prompt for story generation."""
        return template["story_prompt"].format(
            solution=json.dumps(solution, indent=2),
            theme=theme or "default"
        )
    
    def _create_hint_prompt(self, template: Dict, solution: Dict, hint_num: int, total_hints: int) -> str:
        """Create the prompt for hint generation."""
        return template["hint_prompt"].format(
            solution=json.dumps(solution, indent=2),
            hint_number=hint_num + 1,
            total_hints=total_hints
        )
    
    def _create_explanation_prompt(self, template: Dict, solution: Dict) -> str:
        """Create the prompt for solution explanation."""
        return template["explanation_prompt"].format(
            solution=json.dumps(solution, indent=2)
        ) 