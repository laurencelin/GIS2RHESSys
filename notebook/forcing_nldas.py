import os
import time
import shutil
import multiprocessing as mp
from urllib.request import urlopen
from subprocess import Popen, PIPE


class GetForcing:

    def __init__(self):
        print("Start to collect NLDAS forcing data online")

    # read all of the urls defined in the link above
    def create_dir(self):

        input_dir = 'input_files'
        if os.path.exists(input_dir):
            while 1:
                res = input("Directory '{input_dir}' already exists. Do you wish to remove it [Y/N]?")
                if res.lower() == 'y':
                    shutil.rmtree(input_dir)
                    os.mkdir(input_dir)
                    break
                elif res.lower() == 'n':
                    break
                else:
                    print('\nInvalid input. Please answer either Y or N')
        else:
            os.mkdir(input_dir)
        return input_dir

    # write these urls to a local file
    def write_url(self, file_urls, input_dir):

        print('Writing NLDAS urls to {input_dir}/urls.txt')
        f = urlopen(file_urls)
        urls = f.read().decode('utf-8')
        with open('input_files/urls.txt', 'w') as f:
             f.write(urls)

    # define a function to download forcing data
    def get_nldas(self, q, iolock, out_q, cnt):
        while True:
            user, pwd, url, total = q.get()
            if url is None:
                break
            outfile = os.path.join('input_files', url.split('LABEL=')[-1].split('&')[0])
            args = ['wget', '--quiet', '--auth-no-challenge=on', '--content-disposition', '--user', user, '--password',
                    pwd, '-O', outfile, url]
            Popen(args, stdout=PIPE)
            with iolock:
                cnt.value += 1
                percent = cnt.value / total * 100
                print(f'\r[{cnt.value} of {total}] files downloaded -- {percent:.2f}% complete', end=10 * ' ')

    def get_forcing(self, user, password):
        NCORE = 4
        in_q = mp.Queue(maxsize=NCORE)
        out_q = mp.Queue()
        cnt = mp.Value('i', 0)
        iolock = mp.Lock()
        pool = mp.Pool(NCORE, initializer=self.get_nldas,
                       initargs=(in_q, iolock, out_q, cnt))

        with open('input_files/urls.txt', 'r') as f:
            lines = f.readlines()
            total = len(lines)
            for url in lines:
                time.sleep(.1)
                in_q.put((user, password, url, total))  # blocks until q below its max size
        for _ in range(NCORE):  # tell workers we're done
            in_q.put((None, None, None, None))
        pool.close()
        pool.join()