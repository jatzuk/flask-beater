import Foundation

enum Color: String, CaseIterable, Hashable {
  case darkGreen = "DG"
  case pink = "PK"
  case beige = "BE"
  case lightBlue = "LB"
  case brown = "BR"
  case orange = "OR"
  case darkBlue = "DB"
  case red = "RD"
  case lightGreen = "LG"
  case gray = "GY"
  case magenta = "MG"
  case turquoise = "TQ"

  static let maxPiecesPerColor = 4

  var symbol: String { rawValue }
}

enum Piece: Hashable {
  case empty
  case color(Color)
  case hidden(id: Int)

  var isEmpty: Bool {
    if case .empty = self { return true }
    return false
  }

  var color: Color? {
    if case let .color(color) = self { return color }
    return nil
  }
}

struct Flask: Hashable {
  static let capacity = 4
  var pieces: [Piece]

  init(_ pieces: [Piece]) {
    precondition(pieces.count == Flask.capacity, "Each flask must have exactly four slots")
    self.pieces = pieces
  }

  var emptyPieceCount: Int { pieces.filter { $0.isEmpty }.count }

  func topIndex() -> Int? {
    for index in stride(from: Flask.capacity - 1, through: 0, by: -1) {
      if !pieces[index].isEmpty { return index }
    }
    return nil
  }

  func topPiece() -> Piece? {
    guard let index = topIndex() else { return nil }
    return pieces[index]
  }

  var isLocked: Bool {
    guard emptyPieceCount == 0 else { return false }
    guard case let .color(baseColor) = pieces.first else { return false }
    return pieces.allSatisfy { piece in
      if case let .color(color) = piece { return color == baseColor }
      return false
    }
  }
}

struct Move: Hashable {
  let from: Int
  let to: Int
  let count: Int
  let color: Color
}

enum Action: Hashable, CustomStringConvertible {
  case pour(Move)
  case reveal(flaskIndex: Int, color: Color)

  var description: String {
    switch self {
    case .pour(let move):
      return "pour \(move.count) x \(move.color.symbol) from \(move.from + 1) to \(move.to + 1)"
    case .reveal(let flaskIndex, let color):
      return "reveal \(color.symbol) for flask \(flaskIndex + 1)"
    }
  }
}

struct State: Hashable {
  var flasks: [Flask]
}

// MARK: - Helpers

func pieceSymbol(_ piece: Piece) -> String {
  switch piece {
  case .empty:
    return "--"
  case .color(let color):
    return color.symbol
  case .hidden(let id):
    return String(format: "?%d", id)
  }
}

func padded(_ symbol: String, width: Int = 3) -> String {
  if symbol.count >= width { return symbol }
  return symbol + String(repeating: " ", count: width - symbol.count)
}

func printState(_ state: State) {
  let flasksPerRow = 6
  let capacity = Flask.capacity

  for start in stride(from: 0, to: state.flasks.count, by: flasksPerRow) {
    let end = min(start + flasksPerRow, state.flasks.count)
    let slice = state.flasks[start..<end]

    for level in stride(from: capacity - 1, through: 0, by: -1) {
      let line = slice
        .map { padded(pieceSymbol($0.pieces[level])) }
        .joined(separator: " ")
      print(line)
    }
    
    let labels = slice.enumerated()
      .map { index, _ in padded(String(format: "%02d", start + index + 1)) }
      .joined(separator: " ")
    print(labels)

    if end < state.flasks.count { print("") }
  }
  print(String(repeating: "-", count: flasksPerRow * 4))
}

func canonicalKey(for state: State) -> String {
  let flaskStrings = state.flasks.map { flask in
    flask.pieces
      .map { piece -> String in
        switch piece {
        case .empty: return "--"
        case .color(let color): return color.symbol
        case .hidden(let id): return "?\(id)"
        }
      }
      .joined(separator: ",")
  }
  return flaskStrings.joined(separator: "|")
}

func colorUsage(in state: State) -> [Color: Int] {
  var usage: [Color: Int] = [:]
  for color in Color.allCases { usage[color] = 0 }
  for flask in state.flasks {
    for piece in flask.pieces {
      if case let .color(color) = piece {
        usage[color, default: 0] += 1
      }
    }
  }
  return usage
}

func parseColorInput(input: String) -> Color? {
  switch input.uppercased().trimmingCharacters(in: .whitespacesAndNewlines) {
  case "DG": return .darkGreen
  case "PK": return .pink
  case "BE": return .beige
  case "LB": return .lightBlue
  case "BR": return .brown
  case "OR": return .orange
  case "DB": return .darkBlue
  case "RD": return .red
  case "LG": return .lightGreen
  case "GY": return .gray
  default: return nil
  }
}

func isSolved(_ state: State) -> Bool {
  for flask in state.flasks {
    if flask.pieces.contains(where: { if case .hidden = $0 { return true }; return false }) {
      return false
    }

    let colors = flask.pieces.compactMap { piece -> Color? in
      if case let .color(color) = piece { return color }
      return nil
    }

    // empty flasks are OK
    if colors.isEmpty { continue }

    let uniqueColors = Set(colors)
    if uniqueColors.count > 1 { return false }
    if colors.count != 4 { return false }
  }
  return true
}

func applyPour(state: State, from sourceIndex: Int, to targetIndex: Int) -> (State, Move)? {
  guard sourceIndex != targetIndex,
        state.flasks.indices.contains(sourceIndex),
        state.flasks.indices.contains(targetIndex) else { return nil }

  var flasks = state.flasks
  var source = flasks[sourceIndex]
  var target = flasks[targetIndex]

  guard !source.isLocked else { return nil }

  guard let topIdx = source.topIndex(),
        case let .color(color) = source.pieces[topIdx] else { return nil }

  var movableCount = 1
  var cursor = topIdx - 1
  while cursor >= 0 {
    if case let .color(nextColor) = source.pieces[cursor], nextColor == color {
      movableCount += 1
      cursor -= 1
    } else {
      break
    }
  }

  guard movableCount > 0 else { return nil }

  if let targetTop = target.topIndex() {
    switch target.pieces[targetTop] {
    case .color(let existing):
      guard existing == color else { return nil }
    case .hidden:
      return nil
    case .empty:
      break
    }
  }

  let freeSlots = target.emptyPieceCount
  guard freeSlots > 0 else { return nil }

  let transferCount = min(movableCount, freeSlots)
  let insertIndex = target.topIndex().map { $0 + 1 } ?? 0

  guard insertIndex + transferCount <= Flask.capacity else { return nil }

  for offset in 0..<transferCount {
    target.pieces[insertIndex + offset] = .color(color)
  }

  for i in 0..<transferCount {
    source.pieces[topIdx - i] = .empty
  }

  flasks[sourceIndex] = source
  flasks[targetIndex] = target
  let move = Move(from: sourceIndex, to: targetIndex, count: transferCount, color: color)
  return (State(flasks: flasks), move)
}

func apply(_ action: Action, to state: State) -> State? {
  switch action {
  case .pour(let move):
    guard let (nextState, generated) = applyPour(state: state, from: move.from, to: move.to),
          generated.count == move.count,
          generated.color == move.color else { return nil }
    return nextState
  case .reveal(let flaskIndex, let color):
    guard state.flasks.indices.contains(flaskIndex) else { return nil }
    var nextState = state
    let usage = colorUsage(in: state)
    guard usage[color, default: 0] < Color.maxPiecesPerColor else { return nil }
    guard let topIndex = nextState.flasks[flaskIndex].topIndex(),
          case .hidden = nextState.flasks[flaskIndex].pieces[topIndex] else { return nil }
    nextState.flasks[flaskIndex].pieces[topIndex] = .color(color)
    return nextState
  }
}

func findMatchableColors(_ state: State) -> [(Color, Int, Int)] {
  var matches: [(Color, Int, Int)] = []

  for (fromIdx, flask) in state.flasks.enumerated() {
    guard !flask.isLocked,
          let topIdx = flask.topIndex(),
          case let .color(color) = flask.pieces[topIdx] else { continue }

    for (toIdx, targetFlask) in state.flasks.enumerated() {
      guard fromIdx != toIdx, !targetFlask.isLocked, targetFlask.emptyPieceCount > 0 else { continue }

      if let targetTopIdx = targetFlask.topIndex() {
        if case let .color(targetColor) = targetFlask.pieces[targetTopIdx], targetColor == color {
          matches.append((color, fromIdx, toIdx))
        }
      } else if targetFlask.emptyPieceCount == Flask.capacity {
        matches.append((color, fromIdx, toIdx))
      }
    }
  }

  return matches
}

func isUsefulMove(_ state: State, from: Int, to: Int) -> Bool {
  let source = state.flasks[from]
  let target = state.flasks[to]

  if target.emptyPieceCount == Flask.capacity {
    guard let topIdx = source.topIndex(),
          case let .color(color) = source.pieces[topIdx] else { return false }

    var count = 1
    var cursor = topIdx - 1
    while cursor >= 0 {
      if case let .color(c) = source.pieces[cursor], c == color {
        count += 1
        cursor -= 1
      } else { break }
    }

    return count == topIdx + 1 || count == Flask.capacity
  }

  return true
}

func heuristic(_ state: State) -> Int {
  var score = 0

  for flask in state.flasks {
    if flask.isLocked { score -= 100 }
  }

  for flask in state.flasks {
    let colors = Set(flask.pieces.compactMap { $0.color })
    score += colors.count * 10
  }

  score -= state.flasks.reduce(0) { $0 + $1.emptyPieceCount }

  return score
}

func simpleGreedySolve(state: State, path: [Action] = [], visited: inout Set<String>) -> [Action]? {
  if isSolved(state) { return path }

  let key = canonicalKey(for: state)
  if visited.contains(key) { return nil }
  visited.insert(key)

  let matches = findMatchableColors(state)
  if matches.isEmpty {
    for (flaskIdx, flask) in state.flasks.enumerated() {
      guard let topIdx = flask.topIndex(), case .hidden = flask.pieces[topIdx] else { continue }
      let usage = colorUsage(in: state)
      let availableColors = Color.allCases.filter { usage[$0, default: 0] < Color.maxPiecesPerColor }

      for color in availableColors {
        if let nextState = apply(.reveal(flaskIndex: flaskIdx, color: color), to: state) {
          if let solution = simpleGreedySolve(state: nextState, path: path + [.reveal(flaskIndex: flaskIdx, color: color)], visited: &visited) {
            return solution
          }
        }
      }
      return nil
    }
    return nil
  }

  for (_, from, to) in matches {
    if let (nextState, move) = applyPour(state: state, from: from, to: to) {
      let action = Action.pour(move)
      if let solution = simpleGreedySolve(state: nextState, path: path + [action], visited: &visited) {
        return solution
      }
    }
  }

  return nil
}

// MARK: - Parallel BFS Solver
struct SearchNode: Comparable {
  let state: State
  let path: [Action]
  let depth: Int
  let score: Int

  static func < (lhs: SearchNode, rhs: SearchNode) -> Bool {
    lhs.score < rhs.score
  }

  static func == (lhs: SearchNode, rhs: SearchNode) -> Bool {
    lhs.score == rhs.score
  }
}

func parallelBFSSolve(state: State, maxDepth: Int = 100, beamWidth: Int = 300_000) -> [Action]? {
  let queue = DispatchQueue(label: "solver.queue", attributes: .concurrent)
  let visitedQueue = DispatchQueue(label: "solver.visited")
  let resultQueue = DispatchQueue(label: "solver.result")

  var visited = Set<String>()
  var solutionFound: [Action]? = nil

  visited.insert(canonicalKey(for: state))

  var currentLevel = [SearchNode(state: state, path: [], depth: 0, score: heuristic(state))]
  var nodesExplored = 0

  for depth in 0...maxDepth {
    guard solutionFound == nil else { break }
    guard !currentLevel.isEmpty else { break }

    print("Exploring depth \(depth), nodes: \(currentLevel.count), visited: \(visited.count)")

    var nextLevel: [[SearchNode]] = Array(repeating: [], count: currentLevel.count)
    let group = DispatchGroup()

    for (index, node) in currentLevel.enumerated() {
      group.enter()
      queue.async {
        defer { group.leave() }

        resultQueue.sync {
          if solutionFound != nil { return }
        }

        if isSolved(node.state) {
          resultQueue.sync {
            if solutionFound == nil || node.path.count < solutionFound!.count {
              solutionFound = node.path
            }
          }
          return
        }

        var localNextNodes: [SearchNode] = []

        var topHiddenFlask: (Int, Int)? = nil
        for (flaskIdx, flask) in node.state.flasks.enumerated() {
          if let topIdx = flask.topIndex(), case .hidden = flask.pieces[topIdx] {
            topHiddenFlask = (flaskIdx, topIdx)
            break
          }
        }

        if let (flaskIdx, _) = topHiddenFlask {
          let usage = colorUsage(in: node.state)
          let availableColors = Color.allCases.filter { usage[$0, default: 0] < Color.maxPiecesPerColor }

          for color in availableColors {
            if let nextState = apply(.reveal(flaskIndex: flaskIdx, color: color), to: node.state) {
              let key = canonicalKey(for: nextState)
              var shouldAdd = false

              visitedQueue.sync {
                if !visited.contains(key) {
                  visited.insert(key)
                  shouldAdd = true
                }
              }

              if shouldAdd {
                let newNode = SearchNode(
                  state: nextState,
                  path: node.path + [.reveal(flaskIndex: flaskIdx, color: color)],
                  depth: depth + 1,
                  score: heuristic(nextState)
                )
                localNextNodes.append(newNode)
              }
            }
          }
        } else {
          let matches = findMatchableColors(node.state)

          if matches.isEmpty {
            for (flaskIdx, flask) in node.state.flasks.enumerated() {
              let hasHidden = flask.pieces.contains { piece in
                if case .hidden = piece { return true }
                return false
              }

              if hasHidden, let topIdx = flask.topIndex(), case .hidden = flask.pieces[topIdx] {
                let usage = colorUsage(in: node.state)
                let availableColors = Color.allCases.filter { usage[$0, default: 0] < Color.maxPiecesPerColor }

                for color in availableColors {
                  if let nextState = apply(.reveal(flaskIndex: flaskIdx, color: color), to: node.state) {
                    let key = canonicalKey(for: nextState)
                    var shouldAdd = false

                    visitedQueue.sync {
                      if !visited.contains(key) {
                        visited.insert(key)
                        shouldAdd = true
                      }
                    }

                    if shouldAdd {
                      let newNode = SearchNode(
                        state: nextState,
                        path: node.path + [.reveal(flaskIndex: flaskIdx, color: color)],
                        depth: depth + 1,
                        score: heuristic(nextState)
                      )
                      localNextNodes.append(newNode)
                    }
                  }
                }
                break
              }
            }
          } else {
            for (_, from, to) in matches {
              // prune useless moves (but be less aggressive initially)
              if !isUsefulMove(node.state, from: from, to: to) {
                // allow moves to empty flasks if still exploring (depth < 100)
                if depth >= 100 || node.state.flasks[to].emptyPieceCount != Flask.capacity {
                  continue
                }
              }

              if let (nextState, move) = applyPour(state: node.state, from: from, to: to) {
                let key = canonicalKey(for: nextState)
                var shouldAdd = false

                visitedQueue.sync {
                  if !visited.contains(key) {
                    visited.insert(key)
                    shouldAdd = true
                  }
                }

                if shouldAdd {
                  let newNode = SearchNode(
                    state: nextState,
                    path: node.path + [.pour(move)],
                    depth: depth + 1,
                    score: heuristic(nextState)
                  )
                  localNextNodes.append(newNode)
                }
              }
            }
          }
        }

        nextLevel[index] = localNextNodes
      }
    }

    group.wait()

    // flatten next level and apply beam search pruning
    currentLevel = nextLevel.flatMap { $0 }
    nodesExplored += currentLevel.count

    if currentLevel.isEmpty && solutionFound == nil {
      print("⚠️ No more nodes to explore at depth \(depth)")
    }

    if currentLevel.count > beamWidth {
      currentLevel.sort()
      currentLevel = Array(currentLevel.prefix(beamWidth))
    }

    if solutionFound != nil {
      print("✅ Solution found at depth \(depth)!")
      break
    }
  }

  return solutionFound
}

func adaptiveInteractiveSolve(state: State, useParallel: Bool = true) {
  print("=== ADAPTIVE INTERACTIVE SOLVER ===")
  print("Using \(useParallel ? "parallel" : "sequential") solver")

  func solveUntilReveal(state: State, stepOffset: Int) -> (State, [Action], Int)? {
    print("Searching for solution from current state")

    let solution: [Action]?
    if useParallel {
      solution = parallelBFSSolve(state: state)
    } else {
      var visited: Set<String> = []
      solution = simpleGreedySolve(state: state, path: [], visited: &visited)
    }

    if let solution = solution {
      var currentState = state
      var executedMoves: [Action] = []
      var currentStep = stepOffset

      for action in solution {
        if case .reveal = action {
          return (currentState, executedMoves, currentStep)
        }

        if let nextState = apply(action, to: currentState) {
          currentStep += 1
          print("Step \(currentStep): \(action.description)")
          printState(nextState)
          currentState = nextState
          executedMoves.append(action)
        } else {
          print("❌ Error applying move: \(action.description)")
          return nil
        }
      }

      return (currentState, executedMoves, currentStep)
    } else {
      print("❌ Could not find solution from current state.")
      return nil
    }
  }

  var currentState = state
  var totalSteps = 0

  while !isSolved(currentState) {
    guard let (newState, _, stepCount) = solveUntilReveal(state: currentState, stepOffset: totalSteps) else {
      print("Cannot continue solving")
      break
    }

    currentState = newState
    totalSteps = stepCount

    if isSolved(currentState) {
      print("🎉 PUZZLE SOLVED! Total steps: \(stepCount)")
      break
    }

    var foundHidden = false
    for (index, flask) in currentState.flasks.enumerated() {
      if let topIdx = flask.topIndex(), case let .hidden(id) = flask.pieces[topIdx] {
        foundHidden = true

        print("\nStep \(totalSteps + 1): Need to reveal hidden piece ?\(id) in flask \(index + 1)")
        print("Current state:")
        printState(currentState)

        let usage = colorUsage(in: currentState)
        let availableColors = Color.allCases.filter { usage[$0, default: 0] < Color.maxPiecesPerColor }
        let colorList = availableColors.map { $0.symbol }.joined(separator: "/")

        if availableColors.count == 1 {
          let onlyColor = availableColors[0]
          print("Only one color possible: \(onlyColor.symbol)")

          let revealAction = Action.reveal(flaskIndex: index, color: onlyColor)
          if let nextState = apply(revealAction, to: currentState) {
            currentState = nextState
            totalSteps += 1
            print("✅ Auto-revealed \(onlyColor.symbol) for flask \(index + 1)")
            print("Updated state:")
            printState(currentState)
            break
          } else {
            print("Error applying reveal. This shouldn't happen.")
            return
          }
        }

        while true {
          print("What color was revealed? (\(colorList) or 'quit'): ", terminator: "")

          guard let userInput = readLine() else {
            print("Error reading input")
            return
          }

          if userInput.lowercased() == "quit" {
            print("Exiting interactive solver")
            return
          }

          guard let revealedColor = parseColorInput(input: userInput) else {
            print("Invalid color. Please use: \(colorList)")
            continue
          }

          if !availableColors.contains(revealedColor) {
            print("Color \(revealedColor.symbol) is already used 4 times. Available colors: \(colorList)")
            continue
          }

          let revealAction = Action.reveal(flaskIndex: index, color: revealedColor)
          if let nextState = apply(revealAction, to: currentState) {
            currentState = nextState
            totalSteps += 1

            print("✅ Revealed \(revealedColor.symbol) in flask \(index + 1)")
            print("Updated state:")
            printState(currentState)
            break
          } else {
            print("Error applying reveal. This shouldn't happen")
            return
          }
        }
        break
      }
    }

    if !foundHidden {
      print("No hidden pieces found but puzzle not solved. This shouldn't happen")
      break
    }
  }
}

func solve(state: State, useParallel: Bool = true) -> [Action]? {
  if useParallel {
    return parallelBFSSolve(state: state)
  } else {
    var visited: Set<String> = []
    return simpleGreedySolve(state: state, visited: &visited)
  }
}

// MARK: - Puzzle bootstrap
//let initialState = State(flasks: [
//  Flask([.color(.red), .color(.beige), .color(.pink), .color(.darkGreen)]),
//  Flask([.hidden(id: 0), .color(.darkBlue), .color(.brown), .color(.lightBlue)]),
//  Flask([.color(.lightBlue), .color(.darkBlue), .color(.orange), .color(.lightBlue)]),
//  Flask([.color(.red), .color(.lightGreen), .color(.pink), .color(.brown)]),
//  Flask([.color(.beige), .color(.darkGreen), .color(.red), .color(.beige)]),
//  Flask([.color(.lightGreen), .color(.gray), .color(.lightBlue), .color(.darkBlue)]),
//  Flask([.color(.lightGreen), .color(.gray), .color(.darkBlue), .color(.darkGreen)]),
//  Flask([.color(.gray), .color(.red), .color(.orange), .color(.brown)]),
//  Flask([.color(.darkGreen), .color(.lightGreen), .color(.orange), .color(.pink)]),
//  Flask([.color(.beige), .color(.pink), .color(.orange), .color(.brown)]),
//  Flask([.empty, .empty, .empty, .empty]),
//  Flask([.empty, .empty, .empty, .empty])
//])

let initialState  = State(flasks:  [
  Flask([.hidden(id: 0), .color(.orange), .color(.darkGreen), .color(.red)]),
  Flask([.color(.turquoise), .color(.lightGreen), .color(.darkBlue), .color(.brown)]),
  Flask([.hidden(id: 1), .color(.pink), .color(.lightBlue), .color(.gray)]),
  Flask([.hidden(id: 2), .color(.darkBlue), .color(.gray), .color(.beige)]),
  Flask([.hidden(id: 3), .hidden(id: 4), .color(.gray), .color(.orange)]),
  Flask([.color(.turquoise), .color(.red), .color(.lightGreen), .color(.orange)]),
  Flask([.color(.beige), .color(.red), .color(.brown), .color(.darkBlue)]),
  Flask([.hidden(id: 5), .hidden(id: 6), .color(.darkGreen), .color(.beige)]),
  Flask([.color(.turquoise), .color(.red), .color(.darkGreen), .color(.brown)]),
  Flask([.color(.lightGreen), .color(.orange), .color(.lightGreen), .color(.magenta)]),
  Flask([.color(.pink), .color(.lightBlue), .color(.darkGreen), .color(.magenta)]),
  Flask([.color(.pink), .color(.lightBlue), .color(.brown), .color(.magenta)]),
  Flask([.empty, .empty, .empty, .empty]),
  Flask([.empty, .empty, .empty, .empty]),
])

print("Initial state:")
printState(initialState)

if let solution = solve(state: initialState) {
  print("=== BRUTE FORCE SOLUTION ===")
  print("Solved in \(solution.count) steps")

  let actions = solution.enumerated().compactMap { (index, action) -> (Int, Action)? in
    if case .reveal = action { return (index + 1, action) }
    return nil
  }

  if !actions.isEmpty {
    print("This solution assumes these reveals:")
    for (stepIndex, reveal) in actions {
      print("step \(stepIndex) -> \(reveal.description)")
    }
  }
  
  adaptiveInteractiveSolve(state: initialState)
} else {
  print("No brute force solution found")
  adaptiveInteractiveSolve(state: initialState)
}
