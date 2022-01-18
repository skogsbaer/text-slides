#!/usr/bin/env python

# see https://github.com/mcfunley/better-keynote-export
import appscript
from argparse import ArgumentParser
from contextlib import closing
from glob import glob
import itertools
import math
import os
import sys
import shutil
import re

class Options(object):
    def __init__(self, outdir, skip_builds):
        self.outdir = os.path.abspath(outdir)
        self.skip_builds = skip_builds

    @property
    def slidesdir(self):
        return os.path.join(self.outdir, 'slides')

def make_dirs(opts):
    for d in (opts.outdir, opts.slidesdir,):
        if not os.path.isdir(d):
            os.mkdir(d)

def export_keynote(filename, outpath, skip_builds):
    filename = os.path.realpath(filename)  # keynote does not like symlinks
    keynote = appscript.app('Keynote')
    k = appscript.k
    keynote_file = appscript.mactypes.File(filename)
    with closing(keynote.open(keynote_file)) as doc:
        doc.export(as_=k.PDF, to=outpath, with_properties = {
            k.export_style: k.IndividualSlides,
            k.all_stages: not skip_builds,
            k.skipped_slides: False
        })

def run_or_fail(cmd):
    print(f'Running {cmd}')
    ecode = os.WEXITSTATUS(os.system(cmd))
    if ecode != 0:
        print(f'{cmd} failed with exit code {ecode}')
        sys.exit(ecode)

def crop(pdf):
    old = pdf + '-old'
    cropped = os.path.splitext(pdf)[0] + '-crop.pdf'
    if os.path.isfile(old) and os.path.isfile(cropped):
        ecode = os.WEXITSTATUS(os.system(f'diff-pdf {old} {pdf}'))
        if ecode == 0:
            print(f'{pdf} unchanged and {cropped} exists, no need to crop')
            return
    run_or_fail(f'cd {os.path.dirname(pdf)}; pdfcrop {os.path.basename(pdf)}')


def main():
    ap = ArgumentParser()
    ap.add_argument('-k', '--keynote', help="Path to the keynote to convert",
                    required=True)
    ap.add_argument('-o', '--outdir', help="Where to put the output.",
                    required=True)
    ap.add_argument('-sb', '--skip-builds', help='Skip build stages and just output one JPG per slide',
                    action='store_true', dest='skip_builds')
    args = ap.parse_args()
    opts = Options(args.outdir, args.skip_builds)
    make_dirs(opts)
    d = opts.slidesdir
    pdf = os.path.join(d, "out.pdf")
    outpath = appscript.mactypes.File(pdf)
    export_keynote(args.keynote, outpath, opts.skip_builds)
    for pdf in [x for x in os.listdir(d) if re.match(r'slides_\d+\.pdf$', x)]:
        run_or_fail(f'cp {d}/{pdf} {d}/{pdf + "-old"}')
    run_or_fail(f'cd {d} && pdftk out.pdf burst output slides_%03d.pdf')
    for pdf in [x for x in os.listdir(d) if re.match(r'slides_\d+\.pdf$', x)]:
        crop(os.path.join(d, pdf))

if __name__ == '__main__':
    main()
