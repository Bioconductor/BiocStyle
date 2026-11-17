# Task runner for BiocStyle development helpers. See dev/revdep/README.md.

# Render reverse-dependency vignettes with two BiocStyles and diff them,
# e.g. `just revdep-sweep baseline=origin/devel candidate=. n=60`
revdep-sweep *args:
    dev/revdep/run.sh sweep {{args}}

# Rebuild report.md for an existing run, e.g. `just revdep-report run=20261117-101500`
revdep-report *args:
    dev/revdep/run.sh report {{args}}

# Delete the cached R library volumes used by the sweep
revdep-clean-cache:
    docker volume ls -q --filter name=biocstyle-revdep-cache | xargs -r docker volume rm
