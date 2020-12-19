
import sys
from collections import defaultdict
chunks = defaultdict(list)
state = ''
chunkNum = -1
for line in sys.stdin:
    if line.strip() == '@file book.nw':
        print line,
        continue
    if line.split()[:2] == ['@begin', 'code']:
        chunkNum = int(line.split()[2])
        chunks[chunkNum].append(line)
        state = 'INCODE'
    elif line.split()[:2] == ['@end', 'code']:
        assert(int(line.split()[2]) == chunkNum)
        chunks[chunkNum].append(line)
        state = ''
        chunkNum = -1
    elif line.split()[:2] == ['@begin', 'docs']:
        chunkNum = int(line.split()[2])
        chunks[chunkNum].append(line)
        state = 'INDOCS'
    elif line.split()[:2] == ['@end', 'docs']:
        chunks[chunkNum].append(line)
        assert(int(line.split()[2]) == chunkNum)
        state = ''
        chunkNum = -1
    else:
        assert(chunkNum != -1)
        chunks[chunkNum].append(line)

# Now we want to propogate all the language information.


def IsCodeChunk(chunk):
    return chunk[0].split()[:2] == ['@begin', 'code']


def ChunkDefn(chunk):
    for line in chunk:
        if line.split()[:1] == ['@defn']:
            return line.split()[1:]
    print chunk


def ChunkUses(chunk):
    for line in chunk:
        if line.split()[:1] == ['@use']:
            yield line.split()[1:]


def ChunkName(chunk):
    for line in chunk:
        if line.split()[:1] == ['@defn']:
            return line.split()[1:]


def FindChunksByNameList(namelist, chunks):
    for c in chunks.keys():
        if ChunkName(chunks[c]) == namelist:
            yield c


def MakeLanguageMap(chunks):
    needWork = True
    languageMap = {}
    while needWork:
        needWork = False
        for n in chunks.keys():
            if not IsCodeChunk(chunks[n]):
                continue

            if n not in languageMap:
                d = ChunkDefn(chunks[n])
                if len(d) == 1 and d[0].endswith(".ecl"):
                    languageMap[n] = 'ECL'
                    needWork = True
                if len(d) == 1 and d[0].endswith(".c"):
                    languageMap[n] = 'C'
                    needWork = True
                if len(d) == 1 and d[0].endswith(".h"):
                    languageMap[n] = 'C'
                    needWork = True
                if len(d) == 1 and d[0].endswith(".y"):
                    languageMap[n] = 'YACC'
                    needWork = True
                if len(d) == 1 and d[0].endswith(".l"):
                    languageMap[n] = 'LEX'
                    needWork = True
                if len(d) == 1 and d[0].endswith(".pl"):
                    languageMap[n] = 'prolog'
                    needWork = True
                if len(d) == 1 and d[0].endswith(".tests"):
                    languageMap[n] = 'raw'
                    needWork = True
                if len(d) == 1 and d[0].endswith(".filetest"):
                    languageMap[n] = 'raw'
                    needWork = True
            else:
                for u in ChunkUses(chunks[n]):
                    for uid in FindChunksByNameList(u, chunks):
                        assert(
                            uid not in languageMap or languageMap[uid] == languageMap[n])
                        if uid not in languageMap:
                            needWork = True
                        languageMap[uid] = languageMap[n]
    return languageMap


def SetChunkLanguage(chunk, language):
    newChunk = []
    for line in chunk:
        # ignore my previos attempt
        if line.split()[:1] == ['@language']:
            continue
        newChunk.append(line)
        if line.split()[:2] == ['@begin', 'code']:
            newChunk.append('@language ' + language + '\n')
    return newChunk


# add in the @language marker to every chunk we can.
languageMap = MakeLanguageMap(chunks)
for n in chunks.keys():
    if not IsCodeChunk(chunks[n]):
        continue
    if n not in languageMap:
        continue  # ???
    chunks[n] = SetChunkLanguage(chunks[n], languageMap[n])

# now we ENTIRELY REMOVE the @code marker, and change it to text.


def CodeChunkToFigChunk(chunk):
    newChunk = []
    chunkName = ""
    for line in chunk:
        if line.split()[:1] == ['@language']:
            newChunk.append('@nl\n')
            start_listing = '@text \\begin{listing}[H]\n@nl\n@text '
            start_minted = '\\begin{minted}[fontsize=\\footnotesize,frame=lines,mathescape]{'

            language = line.split()[1]
            assert(language)
            end_string = language+'}\n@nl\n'
            #end_string = 'c}\n@nl\n'

            newChunk.append(start_listing + start_minted + end_string)
            continue
        if line.split()[:1] == ['@defn']:
            #newChunk.append('@text // '+line)
            chunkName = ' '.join(line.split()[1:])
            continue
        if line.split()[:1] == ['@use']:
            useName = ' '.join(line.split()[1:]).rstrip()

            # newChunk.append(
            # '@text // insert $\cref{'+useName+'}$ (' + useName + ')\n')

            newChunk.append(
                '@text % insert $\cref{'+useName+'}$ (' + useName + ')\n')

            #newChunk.append('@text // Code from \cref{'+useName+'} (' + useName + ')\n')
            continue
        # transform from code to docs. Easy peasy.
        if line.split()[:2] == ['@begin', 'code']:
            line = line.replace('code', 'docs')
        if line.split()[:2] == ['@end', 'code']:
            newChunk.append('@text \end{minted}\n@nl\n')
            newChunk.append(
                '@text \caption{'+chunkName.replace('_', '\\_')+'}\n@nl\n')
            #newChunk.append('@text \label{'+chunkName.replace(' ', ':')+'}\n@nl\n')
            newChunk.append('@text \label{'+chunkName+'}\n@nl\n')
            newChunk.append('@text \end{listing}\n@nl\n')
            line = line.replace('code', 'docs')
        newChunk.append(line)
    return newChunk


for n in chunks.keys():
    if not IsCodeChunk(chunks[n]):
        continue
    if n not in languageMap:
        continue  # ???
    chunks[n] = CodeChunkToFigChunk(chunks[n])


for n in sorted(chunks.keys()):
    for line in chunks[n]:
        print line,
