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
