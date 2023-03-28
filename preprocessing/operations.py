from auxiliary import under, getpart, getpart2, howmany, num_words, syllables, subtokens


def create_table(lib, phr, meta):
    res = []
    i = 0
    k = 0
    for sen in lib:
        conj = []
        for _ in sen:
            conj.append([0])
        for word in sen:
            n = 0
            for _ in conj:
                if word[6] == str(n) and word[7] == "conjunct":
                    conj[n][0] += 1
                    if conj[n][0]==1:
                        conj[n].append(word)
                    elif conj[n][0]==2:
                        conj[n].append(word)
                    else:
                        conj[n][2] = word
                n += 1
        for j in range(len(conj)):
            if conj[j][0] >= 2:
                res.append([])
                cur = sen[j-1]  # current
                a, b = conj[j][1], conj[j][2]  # L.head, R.head
                if int(cur[6]) == 0:  # if no governor
                    res[i].extend([['0'], [''], [''], [''], [''], [cur[7]]])  # governor.position, governor.word, governor.tag, governor.pos, governor.ms, coordination.label
                else:
                    x = int(sen[int(cur[6])-1][0])
                    if x > int(a[0]) and x > int(b[0]):
                        res[i].append(['R'])  # governor.position
                    elif x < int(a[0]) and x < int(b[0]):
                        res[i].append(['L'])  # governor.position
                    else:
                        res[i].append(['M'])  # governor.position
                    gov = sen[int(sen[j-1][6])-1]
                    res[i].append([gov[1]])  # governor.word
                    res[i].append([gov[4]])  # governor.tag
                    res[i].append([gov[3]])  # governor.pos
                    res[i].append([gov[5]])  # governor.ms
                    res[i].append([cur[7]])  # coordination.label
                res[i].append([cur[2]])  # conjunction.word
                res[i].append([cur[3]])  # conjunction.tag
                res[i].append([cur[4]])  # conjunction.pos
                if cur[5] == '_': # if no ms
                    res[i].append([''])  # conjunction.ms
                else:
                    res[i].append([cur[5]])  # conjunction.ms
                res[i].append([str(conj[j][0])])  # no.conjuncts


                new1 = under(conj[j][1], sen)
                if not isinstance(subtokens(new1, sen), list):
                    tks1a, tks1b, tks_all = subtokens(new1, sen)
                    comp1 = getpart2(phr[k], tks1a, tks1b)
                else:
                    tks_all = subtokens(new1, sen)
                    comp1 = getpart(phr[k], tks_all)

                new2 = under(conj[j][2], sen)
                if not isinstance(subtokens(new2, sen), list):
                    tks2a, tks2b, tks_all2 = subtokens(new2, sen)
                    comp2 = getpart2(phr[k], tks2a, tks2b)
                else:
                    tks_all2 = subtokens(new2, sen)
                    comp2 = getpart(phr[k], tks_all2)


                if not isinstance(subtokens(new1, sen), list):
                    res[i].append([getpart2(phr[k], tks1a, tks1b)])  # L.conjunct
                else:
                    res[i].append([getpart(phr[k], tks_all)])  # L.conjunct
                res[i].append([syllables(' '.join(num_words(tks_all)))[1]])  # L.conjunct.syllabylized
                res[i].append([a[7]])  # L.dep.label
                res[i].append([a[1]])  # L.head.word
                res[i].append([a[4]])  # L.head.tag
                res[i].append([a[3]])  # L.head.pos
                res[i].append([a[5]])  # L.head.ms
                res[i].append([len(comp1.split())])  # L.words
                res[i].append([howmany(new1)])  # L.tokens
                res[i].append([syllables(' '.join(num_words(tks_all)))[0]])  # L.syllables
                res[i].append([len(comp1)])  # L.chars
                if not isinstance(subtokens(new1, sen), list):
                    res[i].append([0])  # is.L.continuous
                else:
                    res[i].append([1])  # is.L.continuous

                if not isinstance(subtokens(new2, sen), list):
                    res[i].append([getpart2(phr[k], tks2a, tks2b)])  # R.conjunct
                else:
                    res[i].append([getpart(phr[k], tks_all2)])  # R.conjunct
                res[i].append([syllables(' '.join(num_words(tks_all2)))[1]])  # R.conjunct.syllabylized
                res[i].append([b[7]])  # R.dep.label
                res[i].append([b[1]])  # R.head.word
                res[i].append([b[4]])  # R.head.tag
                res[i].append([b[3]])  # R.head.pos
                res[i].append([b[5]])  # R.head.ms
                res[i].append([len(comp2.split())])  # R.words
                res[i].append([howmany(new2)])  # R.tokens
                res[i].append([syllables(' '.join(num_words(tks_all2)))[0]])  # R.syllables
                res[i].append([len(comp2)])  # R.chars
                if not isinstance(subtokens(new2, sen), list):
                    res[i].append([0])  # is.R.continuous
                else:
                    res[i].append([1])  # is.R.continuous

                res[i].append([phr[k]])  # sentence
                res[i].append([meta[k][0]])  # sent_id
                res[i].append([meta[k][1]])  # sent.file
                i += 1
        k += 1
    return res
