from unittest import TestCase

import tree_sitter, tree_sitter_brick


class TestLanguage(TestCase):
    def test_can_load_grammar(self):
        try:
            tree_sitter.Language(tree_sitter_brick.language())
        except Exception:
            self.fail("Error loading Brick grammar")
