photoEegData <- function(eeg, fotoFreq = 2){ 
  
  sampleRate = eeg$header.signal$EEG_FP1$samplingrate;
  bt <- butter(4, W =  c(45/sampleRate*2, 55/sampleRate*2), type = 'stop')
  btlow <- butter(8, W = 30/sampleRate*2, type = 'low')
  
  channelCount = length(eeg$signal);
  
  for (j in 1:(channelCount-1))
  {
    eeg$signal[[j]]$data = filter(bt, eeg$signal[[j]]$data)
    eeg$signal[[j]]$data = filter(btlow, eeg$signal[[j]]$data)
  }
  
  
  numberOfStimuli = grep(paste (as.character(fotoFreq) ,""), eeg$events$annotation)[1] # Находим те события, которые отвечают за стимул

  timestart = eeg$events$onset[numberOfStimuli];
  onSet = round(timestart*sampleRate);
  samplesCount = round(sampleRate / fotoFreq);
  input = matrix(data = 0, nrow = channelCount, ncol = samplesCount)
  
  t = onSet: (onSet + 1200)
  
  events = t[eeg$signal[[channelCount]]$data[t] == 8]
  for(i in events)
  {
    buffer = matrix(data = 0, nrow = channelCount, ncol = samplesCount);
    
    for (j in 1:channelCount)
    {
      buffer[j,] = eeg$signal[[j]]$data[i:(i+samplesCount - 1)]
    }
    input = input + buffer;
  }
  input = input/length(events);
  
  return(input)
}



