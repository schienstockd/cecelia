import sys

class LogfileUtils():
  def __init__(self, filepath = None, create_file = True):
    self._filepath = filepath
    
    if filepath is not None:
      # create file?
      if create_file is True:
        with open(self.filepath, 'w') as f:
          pass

  """
  Write to log
  """
  def log(self, log_message):
    print(log_message)
    
    if self.filepath is not None:
      with open(self.filepath, 'a') as f:
        print(log_message, file = f)
  
  """
  Print size of variable
  original: https://stackoverflow.com/a/1094933/1870254
  modified: https://stackoverflow.com/a/51046503
  """
  def sizeof_fmt(self, num, suffix = 'B'):
    for unit in ['', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi', 'Ei', 'Zi']:
        if abs(num) < 1024.0:
            return "%3.1f %s%s" % (num, unit, suffix)
        num /= 1024.0
    return "%.1f %s%s" % (num, 'Yi', suffix)
  
  """
  Get memory usage
  https://stackoverflow.com/a/51046503
  """
  def log_mem_usage(self, context = None, var_names = None, num_vars = 10):
    if var_names is not None:
      for i, x in var_names.items():
        self.log('{:>30}: {:>8}'.format(i, self.sizeof_fmt(sys.getsizeof(x))))
    else if context is not None:
      for name, size in sorted(((name, sys.getsizeof(value)) for name, value in list(
                            context.items())), key = lambda x: -x[1])[:num_vars]:
          self.log('{:>30}: {:>8}'.format(name, self.sizeof_fmt(size)))
  
  """
  Getters
  """
  @property
  def filepath(self):
    return self._filepath

  """
  Setters
  """
  @filepath.setter
  def filepath(self, x):
    self._filepath = x
