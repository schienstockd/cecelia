"""
Helper functions for rounding
"""
# https://stackoverflow.com/a/14092788/13766165
def round_up(x, mult):
    x -= x % -mult
    
    return x

def round_down(x, mult):
    x -= x % mult
    
    return x
