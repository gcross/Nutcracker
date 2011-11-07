#@+leo-ver=5-thin
#@+node:gcross.20111107154810.1426: * @file setup.py
def configuration(parent_package='',top_path=None):
    from numpy.distutils.system_info import get_info, NotFoundError
    from numpy.distutils.misc_util import Configuration

    config = Configuration('flatland',parent_package,top_path)

    lapack_opt = get_info('lapack_opt')

    if not lapack_opt:
        raise NotFoundError('no lapack/blas resources found')

#    config.add_extension('nutcracker.core',
#                         sources='nutcracker/core.f95',
#                         extra_info = lapack_opt
#                        )

    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(**configuration(top_path='').todict())
#@-leo
