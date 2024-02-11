#!/usr/bin/env python
# coding: utf-8

from ortools.sat.python import cp_model
import pandas as pd
import numpy as np
import string
import os
from pyexcel_ods3 import save_data

##### FUNKTIONEN ZUM ERZEUGEN VON SUMMEN ÜBER DIE BUCHSTABEN DER WÖRTER #####

## hier wird die summe zu bestimmten woertern gebildet
# input_wrd = ein String das aus buchstaben A-Za-z besteht und in "" stet
# letter_value ist DataFrame (pandas) mit den Spalten "let" und "value"
def word_sum(input_wrd, letter_value):

    input_wrd_cln = input_wrd.replace(" ", "").lower()

    # colnames checken
    cn_df = letter_value.columns.tolist() # extrahiere colnames
    cn_tf = set(['let','value']).issubset(cn_df)

    if not cn_tf:
        raise ValueError("Colnames müssen 'let' und 'value' sein!")
    #----------------------------------------------------------------

    word_sum = 0
    for i in range(len(input_wrd_cln)):
        iw = input_wrd_cln[i]
        word_sum += letter_value[letter_value["let"] == iw]["value"].iloc[0]

    return word_sum



## Ein DataFrame mit Wörtern wird verwendet um deren Summen zu berechnen
def word2value(word_pd, letter_value):
    
    word_pd['letter_sum'] = word_pd['words'].apply(lambda xx: word_sum(input_wrd = xx, letter_value = letter_value))
    return word_pd


# uebergeordnete funktion um ein file mit werten rauszuspeichern
def create_word_value_excel(word_input_file = "words_input.xlsx", 
                            letter_value_file = "letter_value.xlsx", 
                            output_excel = "word_sum_input.xlsx"):

    word_pd = pd.read_excel(word_input_file)
    letter_value = pd.read_excel(letter_value_file)

    wv_pd = word2value(word_pd, letter_value)
    wv_pd.to_excel(output_excel, sheet_name="word_values")
    
    return wv_pd


## hier wird die matrix erstellt
# word_sum file ist ein excel file in dem die wörter plus die summen stehen
def create_letter_matrix(word_sum):
    #word_sum = pd.read_excel(word_sum_file)

    # es wird eine lettermatrix erstellt.
    # zeilen sind die NB, spalten sind die Buchstaben
    anz_zeilen = word_sum.shape[0]
    letter_matrix = np.zeros((anz_zeilen,26),dtype=int)

    # letter zu pos. DataFrame
    letter_pos = pd.DataFrame({
    'position': range(0, 26),
    'buchstaben': list(string.ascii_lowercase)
    })

# matrix befuellen
    woerter = word_sum["words"].apply(lambda x: x.replace(" ", "").lower()).tolist()
    for zei, val in enumerate(woerter):
        for i, letval in enumerate(val):
            pos_spalte = letter_pos[letter_pos["buchstaben"] == letval]["position"]
            if not pos_spalte.empty: # das kam von chatGPT
                letter_matrix[zei,pos_spalte.iloc[0]] += int(1)

    return letter_matrix





def solve_logical(word_sum_file = "word_sum_input.xlsx", outfile = "loesung.ods"):

    # word sum - hier sind die wörter plus die summen drinnen!
    word_pd = pd.read_excel(word_sum_file)
    wordsum = word_pd["letter_sum"].tolist()
    letters = list(string.ascii_lowercase)

    # eine matrix wird erstellt, wie haeufig jeder buchstabe in einem wort vorkommt
    letter_matrix = create_letter_matrix(word_sum = word_pd)
    
    # optimieren

    model = cp_model.CpModel()

    intvars = np.array([])
    for i, val in enumerate(letters):
        intvars = np.append(intvars, model.NewIntVar(1, 26, val))
    
    # alle muessen unterschiedlich sein
    model.AddAllDifferent(intvars)

    # nebenbedingungen - summe der buchstaben der woerter als matrixmultiplikation
    res_sum = letter_matrix @ intvars

    for i, val in enumerate(res_sum):
        model.Add(val == wordsum[i])


    solver = cp_model.CpSolver()
    status = solver.Solve(model)

    # rausspeichern
    res_dat = []
    for i in intvars:
        res_dat.append(solver.Value(i))


    loesung = pd.DataFrame({'Buchstabe': letters, 'value':  res_dat})

    data = {"Loesung": [loesung.columns.tolist()] + loesung.values.tolist()}
    save_data(outfile, data)

    return data
