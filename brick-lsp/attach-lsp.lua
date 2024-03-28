-- Attach LSP to file for testing purposes
require('util').keymap('gd', '<cmd>lua vim.lsp.buf.definition()<CR>')
vim.lsp.start({
    name = 'brick-lsp',
    cmd = {'target/debug/brick-lsp'},
    root_dir = "."
})

