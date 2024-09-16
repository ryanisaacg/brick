package tree_sitter_brick_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_brick "github.com/tree-sitter/tree-sitter-brick/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_brick.Language())
	if language == nil {
		t.Errorf("Error loading Brick grammar")
	}
}
