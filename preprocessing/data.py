def load_conll(file1, file2, file3):
    with open(file1, 'r') as f1:
        with open(file2, 'r') as f2:
            with open(file3, 'r') as f3:
                lib = []
                for i in f1:
                    lib += [i.split()]
                for i in f2:
                    lib += [i.split()]
                for i in f3:
                    lib += [i.split()]
    n = 0
    lib2 = [[]]
    for i in lib:
        if not i:
            n += 1
            lib2 += [[]]
        else:
            lib2[n] += [i]
    lib2.remove([])
    return lib2


def load_txt(file1, file2, file3):
    with open(file1, 'r') as f1:
        with open(file2, 'r') as f2:
            with open(file3, 'r') as f3:
                phr = []
                for i in f1:
                    phr += [i.strip()]
                for i in f2:
                    phr += [i.strip()]
                for i in f3:
                    phr += [i.strip()]
    return phr


def load_json(file1, file2, file3):
    with open(file1, 'r') as f:
        meta = []
        line = 0
        for i in f:
            if line % 5 == 4:
                meta.append([i.split()[1][1:-1], 'test'])
            line += 1
    with open(file2, 'r') as f:
        line = 0
        for i in f:
            if line % 5 == 4:
                meta.append([i.split()[1][1:-1], 'dev'])
            line += 1
    with open(file3, 'r') as f:
        line = 0
        for i in f:
            if line % 5 == 4:
                meta.append([i.split()[1][1:-1], 'train'])
            line += 1
    return meta


def save(file, tab):
    n = 0
    with open(file, 'w') as f:
        f.write('governor.position\tgovernor.word\tgovernor.tag\tcoordination.label\tconjunction.word\tconjunction.tag\tno.conjuncts\tL.conjunct\tL.conjunct.syllabylized\tL.head.word\tL.head.tag\tL.words\tL.tokens\tL.syllables\tL.chars\tis.L.continuous\tR.conjunct\tR.conjunct.syllabylized\tR.head.word\tR.head.tag\tR.words\tR.tokens\tR.syllables\tR.chars\tis.R.continuous\tsentence\tsent_id\tsent.file\n')
        for i in tab:
            a = 0
            n += 1
            for j in i:
                f.write(str(j[0]))
                if a != 28:
                    f.write('\t')
            f.write('\n')
