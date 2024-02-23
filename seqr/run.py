#!/usr/bin/env python3

import hail as hl

import json
import random
import statistics
import sys
import timeit

from glob import glob
from pathlib import Path
from hail_search.search import (
    search_hail_backend as seqr_run,
    load_globals as seqr_init
)
from typing import Dict


def discover() -> Dict[str, Path]:
    query_dir = str(Path(__file__).parent.resolve() / 'query' / '*.json')
    queries = { f.stem : f for f in [Path(f) for f in glob(query_dir)] }

    if not queries:
        raise FileNotFoundError(f'No queries installed in {query_dir.parent}.')

    return queries


def run_benchmark(name, queries, repeats=5):
    with queries[name].open() as f:
        query = json.load(f)

    print(f'Timing query \'{name}\' with {repeats} repeats.')
    res = timeit.repeat(lambda: seqr_run({ **query }), repeat=repeats, number=1)
    print(f'Initial:\t{res[0]}s')
    print(f'   Mean:\t{statistics.fmean(res[1:])}s')
    print(f'  Stdev:\t{statistics.stdev(res[1:])}')


def separator():
    print('-' * 70)


def benchmark(_args, queries) -> int:
    names = [k for k in queries.keys()]
    random.shuffle(names)

    separator()
    run_benchmark('slow', queries)
    # for n in names:
    #     run_benchmark(n, queries)
    #     separator()


def profile(args, queries) -> int:
    input('Attach profiler and press any key to continue.\n')
    print(f'Running query \'{args.query}\'.')
    with queries[args.query].open() as f:
        seqr_run(json.load(f))


def main() -> int:
    import argparse

    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers()

    queries = discover()

    # benchmark
    parse_benchmark = sub.add_parser('benchmark')
    parse_benchmark.set_defaults(main=benchmark)

    # profile
    parse_profile = sub.add_parser('profile')
    parse_profile.add_argument('query', choices=queries.keys())
    parse_profile.set_defaults(main=profile)

    args = parser.parse_args()

    hl._set_flags(use_new_shuffle='1', lower='1')
    hl.init(backend='spark', idempotent=True)
    seqr_init()

    return args.main(args, queries)


if __name__ == '__main__':
    code = main()
    sys.exit(code)
