using RichRopes
using Documenter

DocMeta.setdocmeta!(RichRopes, :DocTestSetup, :(using RichRopes); recursive=true)

makedocs(;
    modules=[RichRopes],
    authors="Sam Atman <atmanistan@gmail.com> and contributors",
    sitename="RichRopes.jl",
    format=Documenter.HTML(;
        canonical="https://mnemnion.github.io/RichRopes.jl",
        edit_link="trunk",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/mnemnion/RichRopes.jl",
    devbranch="trunk",
    branch="gh-pages",
    versions=["stable" => "v^", "v#.#"],
)
