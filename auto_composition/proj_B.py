from earsketch import *

import random
import math

class Note:
  def __init__(self, note, duration, track):
    self.note     = note
    self.duration = duration
    self.track    = track

  def Render(self, startTime):
    endTime = startTime + self.duration
    fitMedia(self.note, self.track, startTime, endTime)
    return endTime

  def Duration(self):
    return self.duration

class Rest:
  def __init__(self, duration):
    self.duration = duration

  def Render(self, startTime):
    pass

  def Duration(self):
    return self.duration

def HalfRest():
  return Rest(0.5)

def QuarterRest():
  return Rest(0.25)

def EighthRest():
  return Rest(0.125)

def HalfNote(note, track):
  return Note(note, 0.5, track)

def QuarterNote(note, track):
  return Note(note, 0.25, track)

def EighthNote(note, track):
  return Note(note, 0.125, track)

class Section:
  def __init__(self):
    self.tempo        = random.randint(80, 120)
    self.numMeasures  = random.choice([2, 4, 6])
    self.pianoHarmony = Piano(self.numMeasures)
    self.harpMelody   = Harp(self.numMeasures, self.pianoHarmony)
    self.bassLine     = Bass(self.numMeasures, self.pianoHarmony)
    self.drumLine     = Drums(self.numMeasures)

  def Duration(self):
    return self.numMeasures

  def NumTracks(self):
    return self.pianoHarmony.NumTracks() + \
           self.harpMelody.NumTracks() + \
           self.bassLine.NumTracks() + \
           self.drumLine.NumTracks()

  def Render(self, startTime):
    self.pianoHarmony.Render(startTime)
    self.harpMelody.Render(startTime)
    self.bassLine.Render(startTime)
    self.drumLine.Render(startTime)

class Song:
  def __init__(self):
    verse  = Section()
    chorus = Section()
    outro  = Section()
    self.sections = [verse, chorus, verse, chorus, chorus, outro]

  def Duration(self):
    return sum(section.Duration() for section in self.sections)

  def NumTracks(self):
    return max(section.NumTracks() for section in self.sections)

  def Render(self, startTime):
    currStartTime = startTime
    for section in self.sections:
      setTempo(section.tempo)
      section.Render(currStartTime)
      currStartTime += section.Duration()

class Drums:
  track = 6

  def __init__(self, numMeasures):
    self.numMeasures = numMeasures
    self.percussion = [OS_KICK01, OS_SNARE01, OS_CLOSEDHAT01]

  def randProb(self):
    probs = [random.random() * 0.5 for x in range(16)]
    probs[0] = 1
    probs[8] = 1
    return probs

  def NumTracks(self):
    return len(self.percussion)

  def randString(self, probs):
    return ''.join(['0' if prob > random.random() else '-' for prob in probs])

  def Render(self, startTime):
    for (offset, item) in enumerate(self.percussion):
      probs   = self.randProb()
      currStr = self.randString(probs)
      println(currStr)
      makeBeat(item, Drums.track + offset, startTime, self.numMeasures * currStr)

class Instrument:
  def __init__(self, numMeasures):
    self.numMeasures = numMeasures
    self.notes = []

  def Duration(self):
    return self.numMeasures

  def GenKeySignature(self):
    genTonic = random.randint(0, 11)
    scales = [[2, 1, 2, 2, 1, 2], # minor
              [2, 2, 1, 2, 2, 2]] # major
    chosenScale = random.choice(scales)
    degrees = []
    degrees.append(genTonic)
    cnt = genTonic
    for jump in chosenScale:
      cnt += jump
      degrees.append(cnt % 12)

    return degrees

  def Render(self, startTime, notes=None):
    if notes is None:
      notes = self.notes

    currTime = startTime
    for note in notes:
      note.Render(currTime)
      currTime += note.Duration()

    return currTime

class Piano(Instrument):
  tones = [PIANO_A, PIANO_AS, PIANO_B, PIANO_C,
           PIANO_CS, PIANO_D, PIANO_DS, PIANO_E,
           PIANO_F, PIANO_FS, PIANO_G, PIANO_GS]

  track = 3

  def __init__(self, numMeasures):
    assert numMeasures % 2 == 0
    Instrument.__init__(self, numMeasures)
    self.keySignature = self.GenKeySignature()
    chordProgressionChoices = \
      [[1, 5, 6, 4],
       [1, 4, 5, 1],
       [1, 6, 4, 5],
       [1, 4, 3, 6],
       [1, 6, 2, 5],
       [1, 5, 1, 6]]
    progression = random.choice(chordProgressionChoices)

    self.chords = (numMeasures / 2) * [self.GetTriad(self.keySignature, degree - 1) for degree in progression]

  def NumTracks(self):
    return len(self.chords[0])

  def Render(self, startTime):
    first = [x for (x,_,_) in self.chords]
    third = [x for (_,x,_) in self.chords]
    fifth = [x for (_,_,x) in self.chords]

    pieces = [first, third, fifth]

    for (trackOffset, piece) in enumerate(pieces):
      notes = [HalfNote(x, Piano.track + trackOffset) for x in piece]
      Instrument.Render(self, startTime, notes)

  def GetTriad(self, scale, root): # start at 0
    doubleRange = 2 * scale
    return (Piano.tones[doubleRange[root]],     # 1
            Piano.tones[doubleRange[root + 2]], # 3
            Piano.tones[doubleRange[root + 4]]) # 5

class Harp(Instrument):
  tones = [HARP_A, HARP_AS, HARP_B, HARP_C,
           HARP_CS, HARP_D, HARP_DS, HARP_E,
           HARP_F, HARP_FS, HARP_G, HARP_GS]

  conv = {}
  for i in xrange(12):
    conv[Piano.tones[i]] = tones[i]

  track = 1

  def NumTracks(self):
    return 1

  def __init__(self, numMeasures, piano):
    Instrument.__init__(self, numMeasures)
    allowedNotes = [Harp.tones[idx] for idx in piano.keySignature]
    allowedNotes = allowedNotes[1:4] + [allowedNotes[0]] + [allowedNotes[4]] + allowedNotes[5:]
    println(allowedNotes)
    for chord in piano.chords:
      currNotes = []
      currNotes.append(Harp.conv[random.choice(chord)])
      for i in xrange(3):
        idx = int(round(random.triangular(0, 6, 3.3)))
        randNote = allowedNotes[idx]
        currNotes.append(randNote)

      random.shuffle(currNotes)
      for randNote in currNotes:
        self.notes.append(EighthNote(randNote, Harp.track))

class Bass(Instrument):
  # FIXME: BASS_A is the only file that doesn't show up for some reason.
  tones = [r'C:\Users\spillow\Documents\REAPER Media\Survey_Of_Music_Technology\Project_B\ToneGen\Bass_Notes\Bass_A.wav', #BASS_A
           BASS_AS, BASS_B, BASS_C,
           BASS_CS, BASS_D, BASS_DS, BASS_E,
           BASS_F, BASS_FS, BASS_G, BASS_GS]

  conv = {}
  for i in xrange(12):
    conv[Piano.tones[i]] = tones[i]

  track = 2

  def NumTracks(self):
    return 1

  def __init__(self, numMeasures, piano):
    Instrument.__init__(self, numMeasures)
    roots = [Bass.conv[x] for (x,_,_) in piano.chords]
    for root in roots:
      self.notes.append(QuarterNote(root, Bass.track))
      self.notes.append(QuarterRest())

def CreateNoisyAMEffect():
  effect = initEffect('PinkAM')

  input     = createUGen(effect, INPUT)
  pinknoise = createUGen(effect, PINK)
  noisestr  = createUGen(effect, TIMES)
  addnoise  = createUGen(effect, ADD)
  sinemod   = createUGen(effect, SINE)
  atten     = createUGen(effect, TIMES)
  output    = createUGen(effect, OUTPUT)

  connect(effect, input, addnoise, 0, 0)
  connect(effect, pinknoise, noisestr, 0, 0)
  connect(effect, noisestr, addnoise, 0, 1)

  connect(effect, addnoise, atten, 0, 0)
  connect(effect, sinemod,  atten, 0, 1)

  connect(effect, atten, output)

  setParamMin(sinemod, FREQUENCY, 20)
  setParamMax(sinemod, FREQUENCY, 2000)
  setParam(sinemod, FREQUENCY, 400)

  setParamMin(noisestr, VALUE, 0)
  setParamMax(noisestr, VALUE, 1)
  setParam(noisestr, VALUE, 0)

  createControl(effect, sinemod, FREQUENCY, 'modulator frequency')
  createControl(effect, noisestr, VALUE, 'noise strength')

  finishEffect(effect)
  return effect

def RandGenProject():
  init()

  theSong = Song()
  theSong.Render(1)

  numTracks = theSong.NumTracks()
  songDuration = theSong.Duration()

  noiseEffect = CreateNoisyAMEffect()

  # apply effect to section 2 harmony.
  for run in [1, 3]:
    effectedHarmony = theSong.sections[run].bassLine
    startTime = sum(theSong.sections[i].Duration() for i in xrange(run))
    endTime   = startTime + theSong.sections[run].Duration()
    for trackNum in range(effectedHarmony.track, effectedHarmony.track + effectedHarmony.NumTracks()):
      setEffect(trackNum, noiseEffect, 'modulator frequency',
                500, startTime,
                2000, (startTime + endTime) / 2.0)
      setEffect(trackNum, noiseEffect, 'modulator frequency',
                2000, (startTime + endTime) / 2.0,
                20, endTime)
      setEffect(trackNum, noiseEffect, 'noise strength',
                0, startTime,
                1, (startTime + endTime) / 2.0)
      setEffect(trackNum, noiseEffect, 'noise strength',
                1, (startTime + endTime) / 2.0,
                0, endTime)

  for trackNum in range(1, numTracks+1):
    setEffect(trackNum, VOLUME, GAIN, 0, songDuration - 2, -60, songDuration + 1)

  finish()

def main():
  RandGenProject()

main()

