function colorData = ColorDataSort(data,info)


eventLength = length( info.annotation.event);

colorData.Color = zeros(eventLength,2);



channelsCount = 21;
sampleCount = round(info.samplerate(1) * 0.5);
colorEventCount = 0;

for i = 1:eventLength
    name = info.annotation.event(i);
    if strfind(name{1}, '(1)')  >= 1
        colorEventCount = colorEventCount + 1;
        colorData.Color(colorEventCount,1) = 1;
        
        bufer = zeros(channelsCount, sampleCount);
        sampleStart = round(info.samplerate(1) * (info.annotation.starttime(i)-0.05));
        for j = 1:channelsCount
            bufer(j,:) = data{j}(sampleStart:(sampleStart + sampleCount-1));
        end
        colorData.Data{colorEventCount} = bufer;
        
        
    elseif strfind(name{1}, '(2)') >= 1
        colorEventCount = colorEventCount + 1;
        colorData.Color(colorEventCount,2) = 1;
        
        bufer = zeros(channelsCount, sampleCount);
        sampleStart = round(info.samplerate(1) * (info.annotation.starttime(i)-0.05));
        for j = 1:channelsCount
            bufer(j,:) = data{j}(sampleStart:(sampleStart + sampleCount-1));
        end
        colorData.Data{colorEventCount} = bufer;
        
    end
end
colorData.Color = colorData.Color(1:colorEventCount,:);


%% dwt

colorData.TransformData = zeros(colorEventCount, channelsCount*28);
for i = 1: colorEventCount
    for j = 1:channelsCount
        [A1,D1] = dwt(colorData.Data{i}(j,:), 'sym4'); 
        [A2,D2] = dwt(A1, 'sym4'); 
        [A3,D3] = dwt(A2, 'sym4');
        [A4,D4] = dwt(A3, 'sym4');
        meanA1 = mean(A1);
        meanA2 = mean(A2);
        meanA3 = mean(A3);
        meanA4 = mean(A4);
        meanD1 = mean(D1);
        meanD2 = mean(D2);
        meanD3 = mean(D3);
        meanD4 = mean(D4);
        colorData.TransformData(i, (1+(j-1)*28):(j*28)) = ...
            [meanA1      meanD1      meanA2      meanD2      ...
             meanA3      meanD3      meanA4      meanD4      ...
             mean(A1.^2) mean(A1.^2) mean(A2.^2) mean(D2.^2) ...
             mean(A3.^2) mean(D3.^2) mean(A4.^2) mean(D4.^2) ...
             std(A1)     std(D1)     std(A2)     std(D2)     ...
             std(A3)     std(D3)     std(A4)     std(D4)     ...
             meanA1/meanD1           meanA2/meanD2           ...
             meanA3/meanD3           meanA4/meanD4           ...  
            ];
       
    end
end





