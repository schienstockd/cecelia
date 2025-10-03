import re

"""
get a tuple from string

https://stackoverflow.com/a/47803708/13766165
"""
def parse_tuple(string):
  return tuple(map(int, re.findall(r'[0-9]+', string)))
