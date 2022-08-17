import subprocess

class ImagejUtils:
  def __init__(self, fiji_path, scripts_path):
    # init params
    self.fiji_path = fiji_path
    self.scripts_path = scripts_path

  # run ImageJ script
  def run_script(self, script_name, script_params):
     # add ''' to string
    for key, value in script_params.items():
      if isinstance(value, str):
        script_params[key] = f'"{value}"'

    # https://imagej.github.io/scripting/headless
    script_params_string = ', '.join('{} = {}'.format(key, value) for key, value in script_params.items())
    # script_params_string = f"'{script_params_string}'"

    # call fiji
    # https://stackoverflow.com/a/28319191/13766165
    with subprocess.Popen([
      self.fiji_path,
      '--ij2', '--headless', '--console', '--run', f'{script_name}.py', script_params_string
      ], cwd = self.scripts_path, stdout = subprocess.PIPE) as p:
      for line in p.stdout:
        print(line) # process line here
	
  # run donblo segmentation
  def run_donblo(self,
    im_path, blob_channels = list(), donut_channels = list(), cell_radius = 3.0,
    gaussian_filter = 1, median_filter = 1, minimum_filter = 1, maximum_filter = 1,
    detection_thresh_adj = 0.4, filtering_after_seg = 0, rolling_radius = 10, seg_path = '',
    post_variance_filter = 4, do_segment = 1, logFile = None):

    # prepare parameters
    script_params = {
      'imPath': im_path,
      'segPath': seg_path,
      'cellRadius': cell_radius,
      'blobChannels': blob_channels,
      'donutChannels': donut_channels,
      'gaussianFilter': gaussian_filter,
      'medianFilter': median_filter,
      'maximumFilter': maximum_filter,
      'minimumFilter': minimum_filter,
      'detectionThreshAdj': detection_thresh_adj,
      'filteringAfterSeg': filtering_after_seg,
      'rollingRadius': rolling_radius,
      'doSegment': do_segment
      # 'postVarianceFilter': post_variance_filter
    }
    
    self.run_script('runDonblo', script_params)
