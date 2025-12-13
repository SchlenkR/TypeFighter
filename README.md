# TypeFighter

> **Companion repository for the YouTube video:** [Type Inference Explained](https://www.youtube.com/watch?v=fSRTVrjvo70)

TypeFighter is an educational type inference system implementation in F# that demonstrates how type systems work internally. This repository allows you to explore and experiment with type inference concepts interactively.

## 🎯 Purpose

This project helps you understand type inference by:
- Providing runnable examples that visualize type inference steps
- Demonstrating various type system features (polymorphism, records, pattern matching, etc.)
- Offering an interactive AST visualization tool

## 🚀 Getting Started

### Prerequisites

- [.NET SDK](https://dotnet.microsoft.com/download) (version 8.0 or higher)
- [VS Code](https://code.visualstudio.com/) with [Ionide](https://ionide.io/) extension
- [Node.js](https://nodejs.org/) and npm

### Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/SchlenkR/TypeFighter.git
   cd TypeFighter
   ```

2. **Restore .NET dependencies**
   ```bash
   dotnet restore
   ```

3. **Install visualization dependencies**
   ```bash
   npm install
   ```

4. **Start the visualization server**
   ```bash
   npm start
   ```
   This launches the AST visualizer at `http://localhost:5173` (or similar).

### Running Examples

#### Start Here: The Introduction Example

1. Open `src/TypeFighter.Tests/video_00/the-example.fsx` in VS Code
2. Select the top section (the setup code)
3. Press `Alt+Enter` (or `Cmd+Enter` on macOS) to evaluate it in F# Interactive
4. Now evaluate each small program block individually to see type inference in action
5. Check the visualization webpage to see the AST and type inference steps

#### Explore More Examples

The `src/TypeFighter.Tests/TestCases/` directory contains comprehensive examples:

- `Base.fs` - Basic type inference
- `Polymorphism.fs` - Generic types and polymorphism
- `Records.fs` - Record types
- `Arrays.fs` - Array and collection types
- `PatternMatching.fs` - Pattern matching with types
- `IntersectionTypes.fs` - Intersection type features
- `TypeHierarchies.fs` - Type hierarchies and subtyping
- `Misc.fs` - Additional scenarios

All test files are executable in F# Interactive following the same pattern as `the-example.fsx`.

## 📁 Project Structure

```
TypeFighter/
├── src/
│   ├── TypeFighter/          # Core type inference engine
│   │   ├── Lang.fs           # Language definition and types
│   │   ├── ExpressionDsl.fs  # DSL for building expressions
│   │   └── Utils.fs          # Utilities
│   ├── TypeFighter.Tests/    # Examples and test cases
│   │   ├── video_00/         # Video companion examples
│   │   └── TestCases/        # Feature examples
│   └── visu/                 # AST visualization tool (TypeScript)
├── package.json              # npm dependencies for visualization
└── TypeFighter.sln           # .NET solution file
```

## 🔧 Building

```bash
dotnet build
```

Or use the VS Code build task: `TypeFighter (NonGraphBased): build dotnet solution`

## 🎨 Visualization

The visualization tool displays the Abstract Syntax Tree (AST) and type inference process. After starting the dev server with `npm start`, it automatically updates as you evaluate expressions in F# Interactive.

## 📜 License

**⚠️ Important:** This project is **NOT available for use** in any projects, whether commercial or non-commercial. 

If you are interested in using this project, please [contact me](https://github.com/SchlenkR) directly for permission.

## 🙏 Acknowledgments

This project was created as educational material to accompany the type inference video tutorial. Feel free to explore, learn, and experiment with the code for educational purposes.

---

**Questions or feedback?** Feel free to open an issue or reach out!
