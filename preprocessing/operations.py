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
                cur = sen[j-1]
                a, b = conj[j][1], conj[j][2]  # heads
                if int(cur[6]) == 0:  # no governor
                    res[i].extend([['0'], [''], [''], [''], [cur[7]]])  # adds 0, NA, NA, NA, root
                else:
                    x = int(sen[int(cur[6])-1][0])
                    if x > int(a[0]) and x > int(b[0]):
                        res[i].append(['R'])
                    elif x < int(a[0]) and x < int(b[0]):
                        res[i].append(['L'])
                    else:
                        res[i].append(['M'])
                    gov = sen[int(sen[j-1][6])-1]
                    res[i].append([gov[1]])  # governor
                    res[i].append([gov[3]])  # gov tag
                    res[i].append([gov[4]])  # full gov tag
                    res[i].append([cur[7]])  # coord eti
                res[i].append([str(conj[j][0])])# liczba członów
                res[i].append([cur[2]])  # conj
                res[i].append([cur[3]])  # cat of conj
                new1 = under(conj[j][1], sen)
                if not isinstance(subtokens(new1, sen), list):
                    tks1a, tks1b, tks_all = subtokens(new1, sen)
                    comp1 = getpart2(phr[k], tks1a, tks1b)
                else:
                    tks_all = subtokens(new1, sen)
                    comp1 = getpart(phr[k], tks_all)
                res[i].append([len(comp1.split())])  # a: no of words
                res[i].append([howmany(new1)])  # a: no of tokens
                res[i].append([syllables(' '.join(num_words(tks_all)))[0]])  # a: no of syllables
                res[i].append([len(comp1)])  # a: no of chars
                if not isinstance(subtokens(new1, sen), list):
                    res[i].append([getpart2(phr[k], tks1a, tks1b)])
                    res[i].append([0])
                else:
                    res[i].append([getpart(phr[k], tks_all)])  # a: text
                    res[i].append([1])  # a: czy ciagly?
                res[i].append([syllables(' '.join(num_words(tks_all)))[1]])  # a: text divided
                res[i].append([a[1]])  # a: head
                res[i].append([a[3]])  # a: tag of head
                res[i].append([a[4]])  # a: full tag of head
                new2 = under(conj[j][2], sen)
                if not isinstance(subtokens(new2, sen), list):
                    tks2a, tks2b, tks_all2 = subtokens(new2, sen)
                    comp2 = getpart2(phr[k], tks2a, tks2b)
                else:
                    tks_all2 = subtokens(new2, sen)
                    comp2 = getpart(phr[k], tks_all2)
                res[i].append([len(comp2.split())])
                res[i].append([howmany(new2)])  # b: no of tokens
                res[i].append([syllables(' '.join(num_words(tks_all2)))[0]])  # b: no of sylables
                res[i].append([len(comp2)])  # b: no of chars
                if not isinstance(subtokens(new2, sen), list):
                    res[i].append([getpart2(phr[k], tks2a, tks2b)])
                    res[i].append([0])
                else:
                    res[i].append([getpart(phr[k], tks_all2)])  # b: text
                    res[i].append([1])  # b: czy ciagly?
                res[i].append([syllables(' '.join(num_words(tks_all2)))[1]])  # b: text divided
                res[i].append([b[1]])  # b: head
                res[i].append([b[3]])  # b: tag of head
                res[i].append([b[4]])  # b: full tag of head
                res[i].append([phr[k]])  # full sentence
                res[i].append([meta[k][0]])  # sentence id
                res[i].append([meta[k][1]])  # from what file
                i += 1
        k += 1
    return res
