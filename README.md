# xtb-instruments

A simple application for creating lists of instruments available for trading on
XTB. It saves a list of instruments of a given type to a text file and allows
updating it later in case of changes.

The available instruments are retrieved from this website:
https://www.xtb.com/pl/oferta/informacje-o-rachunku/specyfikacja-instrumentow

## Building

This is a [Stack](https://docs.haskellstack.org/) project, you can use standard
Stack commands to build and run.

```
stack build
```

## Usage

```
Usage: xtb-instruments INSTRUMENT_TYPE [-c|--country COUNTRY]
                       [-o|--output FILENAME]

  XTB instrument list downloader

Available options:
  INSTRUMENT_TYPE          Instrument type. One of: forex, indices, cashstocks,
                           commodities, shares, cryptocurrencies, etfcfd, etfs.
  -c,--country COUNTRY     Country - necessary for cashstocks and shares
                           instrument types. One of: us, pl, uk, ukint, cz, fr,
                           de, it, pt, es, be, nl, ch, dk, se, fi, no.
  -o,--output FILENAME     Output filename. Default:
                           xtb_<instrument_type>_<country>.txt
  -h,--help                Show this help text
```

### Examples 

Downloading a list of Forex currency pairs to `forex.txt`:

```
stack exec xtb-instruments -- forex -o forex.txt
```

Downloading a list of Polish stocks to `xtb_cashstocks_PL.txt`:

```
stack exec xtb-instruments -- cashstocks -c pl
```

If the target file already exists, it will be modified to match instruments
currently found on the website and information about any changes will be printed
to standard output.
