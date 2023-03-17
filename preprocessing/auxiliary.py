import re
import pyphen
import num2words


def under(word, sentence):
    li = []
    for i in range(len(sentence)):
        li.append(0)
        if word[0] == sentence[i][0]:
            li[i] = 1
    for i in range(len(sentence)):
        if sentence[i][6] == word[0]:
            li[i] = 1
            lista2 = under(sentence[i], sentence)
            for j in range(len(lista2)):
                if lista2[j] == 1:
                    li[j] = 1
    return li


def getpart(sentence, tokens_list):
    tokens_list2 = [re.escape(token) for token in tokens_list]
    pattern = "(" + " *".join(tokens_list2) + ")"
    matches = re.finditer(pattern, sentence)
    match_list = [match.group() for match in matches]
    if len(match_list) > 1:
        match_length = len(match_list[0])
        for match in match_list:
            assert len(match) == match_length, "Error: length is not the same (problem with spaces)"
    if len(match_list) == 0:
        return "Error: problem with words in between"
    return match_list[0]

def getpart2(sentence, tokens_list1, tokens_list2):
    tokens_1 = [re.escape(token) for token in tokens_list1]
    tokens_2 = [re.escape(token) for token in tokens_list2]
    pattern = "(" + " *".join(tokens_1) + ")"
    pattern2 = "(" + " *".join(tokens_2) + ")"
    matches = re.finditer(pattern, sentence)
    matches2 = re.finditer(pattern2, sentence)
    match_list = [match.group() for match in matches]
    match_list2 = [match.group() for match in matches2]
    if len(match_list) > 1:
        match_length = len(match_list[0])
        for match in match_list:
            assert len(match) == match_length, f"Error: {sentence}, {match}, {len(match)}, {match_list[0]}, {match_length}"
    if len(match_list2) > 1:
        match_length2 = len(match_list2[0])
        for match in match_list2:
            assert len(match) == match_length2, f"Error:{sentence}, {match}, {len(match)}, {match_list2[0]}, {match_length2}"
    if len(match_list) == 0:
        return "Error: problem with words in between"
    if len(match_list2) == 0:
        return "Error: problem with words in between"
    return match_list[0] + ' ' + match_list2[0]


def howmany(li):
    res = 0
    for i in li:
        if i == 1:
            res += 1
    return res


def syllables(text):
    dic = pyphen.Pyphen(lang='pl_PL')
    n = 1
    tildes = dic.inserted(text, "~")
    for i in tildes:
        if i == '~':
            n += 1
    return n, tildes


def subtokens(binary, tokens):
    tks1 = []
    tks1a = []
    tks1b = []
    tks_all = []
    start = None
    for c in range(len(binary)):
        if binary[c] == 1:
            tks_all.append(tokens[c][1])
            if start is None:
                start = c
            if c == len(binary) - 1 or binary[c + 1] == 0:
                tks1.extend([tokens[i][1] for i in range(start, c + 1)])
                if not tks1a:
                    tks1a = tks1
                    assert not tks1b
                else:
                    tks1b = tks1
                tks1 = []
                start = None
    if tks1b:
        return tks1a, tks1b, tks_all
    else:
        return tks_all


def num_words(tks):
    tks2 = [tokens for tokens in tks]
    for i in range(len(tks2)):
        if tks2[i].isnumeric():
            tks2[i] = num2words.num2words(tks2[i], lang='pl')
    return tks2
