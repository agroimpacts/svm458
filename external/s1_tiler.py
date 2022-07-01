import sys
import os
import importlib

sys.path.append('../../cscdc/scripts/s1_time_series/')
import s1_time_series
importlib.reload(s1_time_series)
from s1_time_series import *

def _load_yaml(config_path, logger=None):
    """Load config yaml file
    Args:
        config_path (str): the path of config yaml.
        logger (logging.Logger): the logger object to store logs.
    Returns:
        dict: config, a dictionary of configs.
    """
    try:
        with open(config_path, 'r') as yaml_file:
            config = yaml.safe_load(yaml_file)
            return config
    except OSError:
        if logger is None:
            print("Cannot open", config_path)
        else:
            logger.error("Cannot open", config_path)
        raise
log = True
yaml_path = './config_s1_tiler.yaml'
config = _load_yaml(yaml_path)


# Set up logger
def _setup_logger(log_dir, use_date=False, log_name = 's1_tiler'):
    if use_date:
        dt = datetime.now().strftime("%d%m%Y_%H%M")
        log = "{}/{}_{}.log".format(log_dir, log_name, dt)
    else: 
        log = "{}/{}.log".format(log_dir, log_name)
        
    for handler in logging.root.handlers[:]:
        logging.root.removeHandler(handler)
    log_format = "%(asctime)s::%(levelname)s::%(name)s::%(filename)s::%(lineno)d::%(message)s"
    logging.basicConfig(filename=log, filemode='w',
                        level=logging.INFO, format=log_format)
    
    return logging.getLogger()

# directory parameters
dst_dir = config['dst_dir']
log_dir = os.path.join(dst_dir, config['log_dir'])

# s1 time series directory
ts_dir = os.path.join(dst_dir, config['time_series_path'])
if not os.path.isdir(ts_dir):
    os.mkdir(ts_dir)

# log directory and logger
if log:
    dst_dir = config['dst_dir']
    log_dir = os.path.join(dst_dir, config['log_dir'])
    if not os.path.isdir(log_dir):
        os.mkdir(log_dir)
    logger = _setup_logger(log_dir, use_date=True)
    logger.info(f'Initializing S1 tiling logs /n')


# load in tile and S1 footprint catalogs
tiles = gpd.read_file(config['tiles_path'])[['tile', 'geometry']]
s1_footprints = gpd.read_file(config['catalog_path'])

if log:
    logger.info(f'Read in catalogs \n')


# Run time series
tids = tiles['tile'].unique().tolist()
# if test:
#     random.seed(10)
#     tids = random.sample(tids, k=40)
#     print(tids)

# tid_completed = []

gdalversion = True
skip_created = True
# logger = None

tid = 539119
tile = tiles[tiles['tile'] == tid]
tile


if __name__  == "__main__":
    test = True
    log = True
    gdalversion = True
    skip_created = True
    s1_tile_ts(tile, s1_footprints, config, logger, gdalversion, 
        skip_created)
