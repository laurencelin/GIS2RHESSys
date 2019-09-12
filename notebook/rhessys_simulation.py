import subprocess
import shlex
import itertools
import numpy as np

def execute(executable, start_date, end_date, config, path):

    default_cmd = '-b -newcaprise -capr 0.001 -gwtoriparian -capMax 0.01 -slowDrain -leafDarkRespScalar 0.5   -frootRespScalar 0.25 -StemWoodRespScalar 0.05'

    list_keys = [k for k in config]
    p = list_keys[0].split("++")[1:len(list_keys[0].split("++")) - 1]

    parameter = []
    name = []

    for i in list_keys:
        name.append(i)

    for i, value in enumerate(list_keys):
        parameter.append(list_keys[i].split("++")[1:len(list_keys[i].split("++")) - 1])

    ensemble_rhessys_run_cmd = []
    for i, k in enumerate(list_keys):
        rhessys_run_cmd = '{} -st {} -ed {} {} -t {}tecfiles/tec_daily.txt ' \
                             '-w {}worldfiles/worldfile -whdr {}worldfiles/worldfile.hdr ' \
                             '-r {}flows/flowtable.txt -rtz 2.7 -pre {}output/{}' \
            .format(executable, start_date, end_date, default_cmd,
                    path, path, path, path, path, name[i])
        ensemble_rhessys_run_cmd.append(rhessys_run_cmd)
        
    my_dict = {'s1': 2.9, 's2': 1.4, 's3': 20.0, 'sv1': 4.5, 'sv2': 55.6, 'gw1': 0.05, 'gw2': 0.1}
    ensemble_parameters = []
    for i, k in enumerate(parameter):
        for j in parameter[i]:
            keys = j.split("=")[0]
            values = j.split("=")[1]
            my_dict[keys] = values
        par = ' -s ' + str(my_dict['s1']) + ' ' + str(my_dict['s2']) + ' ' + str(my_dict['s3']) + ' -sv ' \
              + str(my_dict['sv1']) + ' ' + str(my_dict['sv2']) + ' -gw ' + str(my_dict['gw1']) + ' ' + str(my_dict['gw2'])
        ensemble_parameters.append(par)
        
    for i, k in enumerate(list_keys):
        cmd = ensemble_rhessys_run_cmd[i] + ' ' + ensemble_parameters[i]
        print(cmd)
        cmd_rhessys = shlex.split(ensemble_rhessys_run_cmd[i] + ensemble_parameters[i])
        process = subprocess.Popen(cmd_rhessys, stdout=subprocess.PIPE)
        output = process.communicate()[0].decode('utf-8')

    return name

def product_dict(**kwargs):
    """
    Take a set of dictionary arguments and generate a new set of
    dictionaries that have all combinations of values for each key.
    """
    keys, vals = kwargs.keys(), kwargs.values()
    for instance in itertools.product(*vals):
        yield dict(zip(keys, instance))


def parameter_product(list_config):
    return {'++'+'++'.join('{}={}'.format(k, v) for k, v in d.items())+'++':
            {'parameters': d} for d in product_dict(**list_config)}

def safe_arange(start, stop, step):
    a = np.arange(start, stop, step)
    result =[]
    for i in a:
        par = round(i, 10)
        result = np.append(result, par)
    return result
