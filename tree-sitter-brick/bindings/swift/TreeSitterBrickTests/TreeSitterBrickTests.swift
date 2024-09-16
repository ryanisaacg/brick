import XCTest
import SwiftTreeSitter
import TreeSitterBrick

final class TreeSitterBrickTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_brick())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Brick grammar")
    }
}
