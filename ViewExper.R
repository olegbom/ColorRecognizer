len = length (eeg$events$annotation)


numberOfStimuli = grep("\\([12]\\)", eeg$events$annotation)
stimuliCount = length(numberOfStimuli)

furiePointsCount = 70;
output = numeric(stimuliCount)
input = matrix(data = 0, nrow = stimuliCount, ncol = 21*furiePointsCount*2)

sampleRate = eeg$header.signal$EEG_Fp1$samplingrate;
samplesCount = round(sampleRate * 0.1)

counter = 0;

for (i in numberOfStimuli)
{
  counter = counter + 1
  eegSample = numeric(21*furiePointsCount*2)
  onSet = round((eeg$events$onset[i]-0.05)*sampleRate)
  for (j in 1:21)
  {
    buffer = eeg$signal[[j]]$data[onSet:(onSet + samplesCount-1)]
    furie = fft(buffer, inverse = FALSE)
    eegSample[(1 + (j-1)*furiePointsCount*2): (j*furiePointsCount*2 - furiePointsCount)] = log10(Mod(furie[1:furiePointsCount]))
    eegSample[(1 + j*furiePointsCount*2 - furiePointsCount): (j*furiePointsCount*2) ] = Arg(furie[1:furiePointsCount])
  }
  
  input[counter,] = eegSample
  output[counter] = ifelse( grepl("\\(1\\)", eeg$events$annotation[i]), 1,0);
}
