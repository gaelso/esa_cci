import cdsapi

c = cdsapi.Client()

c.retrieve(
    'satellite-land-cover',
    {
        'variable': 'all',
        'format': 'zip',
        'year': [
             '2015'
        ],
        'version': [
            'v2.0.7cds', 'v2.1.1',
        ],
    },
    'download.zip')
