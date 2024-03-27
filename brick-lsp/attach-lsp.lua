-- Attach LSP to file for testing purposes
vim.lsp.start({
    name = 'brick-lsp',
    cmd = {'target/debug/brick-lsp'},
    root_dir = "."
})
