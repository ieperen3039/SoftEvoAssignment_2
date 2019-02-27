module GoldenStandardReader

import IO;
import lang::csv::IO;

import DataTypes;

Clone readGoldenStandardSmall() = readCSV(#Clone, |project://assignment2/data/sample_small.csv|);
Clone readGoldenStandardLarge() = readCSV(#Clone, |project://assignment2/data/sample_large.csv|);