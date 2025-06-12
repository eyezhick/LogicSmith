import streamlit as st
import sys
from pathlib import Path

# Add the project root to the Python path
project_root = Path(__file__).parent.parent
sys.path.append(str(project_root))

from python.engine.puzzle_engine import PuzzleEngine
from python.narrator.story_generator import StoryGenerator

def initialize_session_state():
    """Initialize session state variables."""
    if "puzzle" not in st.session_state:
        st.session_state.puzzle = None
    if "story" not in st.session_state:
        st.session_state.story = None
    if "current_hint" not in st.session_state:
        st.session_state.current_hint = 0
    if "show_solution" not in st.session_state:
        st.session_state.show_solution = False

def main():
    st.set_page_config(
        page_title="LogicSmith",
        page_icon="🧩",
        layout="wide"
    )
    
    initialize_session_state()
    
    st.title("🧩 LogicSmith")
    st.markdown("""
    A hybrid symbolic–neural framework for procedural generation and pedagogical presentation of logic puzzles.
    """)
    
    # Sidebar for puzzle generation
    with st.sidebar:
        st.header("Generate Puzzle")
        
        puzzle_type = st.selectbox(
            "Puzzle Type",
            ["zebra", "kenken", "nonogram"],
            index=0
        )
        
        difficulty = st.selectbox(
            "Difficulty",
            ["easy", "medium", "hard"],
            index=1
        )
        
        theme = st.text_input(
            "Theme (optional)",
            placeholder="e.g., detective, fantasy, sci-fi"
        )
        
        if st.button("Generate New Puzzle"):
            with st.spinner("Generating puzzle..."):
                engine = PuzzleEngine()
                st.session_state.puzzle = engine.generate(
                    puzzle_type=puzzle_type,
                    difficulty=difficulty,
                    theme=theme
                )
                
                narrator = StoryGenerator()
                st.session_state.story = narrator.generate_story(
                    puzzle_type=puzzle_type,
                    solution=st.session_state.puzzle["solution"],
                    theme=theme,
                    difficulty=difficulty
                )
                st.session_state.current_hint = 0
                st.session_state.show_solution = False
    
    # Main content area
    if st.session_state.puzzle and st.session_state.story:
        # Display the story
        st.header("The Story")
        st.write(st.session_state.story["story"])
        
        # Display hints
        st.header("Hints")
        if st.session_state.current_hint < len(st.session_state.story["hints"]):
            if st.button("Show Next Hint"):
                st.session_state.current_hint += 1
            
            for i in range(st.session_state.current_hint):
                st.info(f"Hint {i + 1}: {st.session_state.story['hints'][i]}")
        else:
            st.success("All hints have been revealed!")
        
        # Solution section
        st.header("Solution")
        if st.button("Show Solution"):
            st.session_state.show_solution = True
        
        if st.session_state.show_solution:
            st.write(st.session_state.story["explanation"])
            
            # Display the solution in a structured format
            st.subheader("Final Answer")
            solution = st.session_state.puzzle["solution"]
            for house in solution:
                st.write(f"House {house['number']}:")
                st.write(f"- Color: {house['color']}")
                st.write(f"- Nationality: {house['nationality']}")
                st.write(f"- Drink: {house['drink']}")
                st.write(f"- Cigarette: {house['cigarette']}")
                st.write(f"- Pet: {house['pet']}")
                st.write("---")
    else:
        st.info("Use the sidebar to generate a new puzzle!")

if __name__ == "__main__":
    main() 