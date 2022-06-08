import std/[pegs, strutils]

let semverPeg = peg"""
semver <- core ("-" pre ("+" build)? / "+" build)?
core <- major "." minor "." patch
major <- num_id
minor <- num_id
patch <- num_id
pre <- dot_sep_pre_id
dot_sep_pre_id <- pre_id ("." dot_sep_pre_id)?
build <- dot_sep_build_id
dot_sep_build_id <- build_id ("." dot_sep_build_id)?
pre_id <- alpha_id / num_id
build_id <- alpha_id / digits
alpha_id <- non_digit id_chars? / id_chars non_digit id_chars?
num_id <- "0" / positive_digit digits?
id_chars <- id_char+
id_char <- digit / non_digit
non_digit <- letter / "-"
digits <- digit+
digit <- [0-9]
positive_digit <- [1-9]
letter <- [a-zA-Z]
"""

type SemVer* = tuple
  major: int
  minor: int
  patch: int
  pre_release: seq[string]
  build: seq[string]

proc semver*(ver: string): SemVer =
  var r: SemVer
  let parser = semverPeg.eventParser:
    pkNonTerminal:
      # enter: debugecho "? " & p.nt.name
      leave:
        if length > 0:
          # debugecho "= " & p.nt.name
          template match: string = s.substr(start, start+length-1)
          case p.nt.name
          of "major":
            r.major = match().parseInt
          of "minor":
            r.minor = match().parseInt
          of "patch":
            r.patch = match().parseInt
          of "pre":
            r.pre_release = match().split('.')
          of "build":
            r.build = match().split('.')
        # else: debugecho "! " & p.nt.name
  doAssert parser(ver) == ver.len
  r

func `<`(a, b: seq[string]): bool =
  var i = a.len
  while i < a.len and i < b.len:
    if a[i] < b[i]:
      i.inc
    else: return false
  true

func `<`*(a, b: SemVer): bool =
  a.major < b.major or
  (a.major == b.major and a.minor < b.minor) or
  (a.major == b.major and a.minor == b.minor and a.patch < b.patch) or
  (a.major == b.major and a.minor == b.minor and a.patch == b.patch and a.pre_release < b.pre_release)

when isMainModule and defined test:
  import std/unittest
  check semver"1.0.0-alpha" < semver"1.0.0-alpha.1"
  check semver"1.0.0-alpha.1" < semver"1.0.0-alpha.beta"
  check semver"1.0.0-alpha.beta" < semver"1.0.0-beta"
  check semver"1.0.0-beta" < semver"1.0.0-beta.2"
  check semver"1.0.0-beta.2" < semver"1.0.0-beta.11"
  check semver"1.0.0-beta.2.0" < semver"1.0.0-beta.11"
  check semver"1.0.0-beta.11" < semver"1.0.0-rc.1"
  check semver"1.0.0-rc.1" < semver"1.0.0"
  check semver"1.0.0" > semver"1.0.0-rc.1"
  check semver"1.0.0" >= semver"1.0.0-rc.1"
  check semver"1.0.0" == semver"1.0.0"
  check not (semver"1.0.1" < semver"1.0.0")
