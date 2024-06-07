% Obtain all review (.mat files) names in selected directory is made
% in .mlapp, app and script share workspace so files is defined
% previously

% Variables preallocate
DTF_Results=[];
DTF_Results_Replicates=[];
DTF_Combs=[];
DTF_Times=[];
Correct_Replicates=[];
Evaluated_RCombs=[];
Total_nRComb=[];
Correct=[];
Evaluated_GCombs=[];
Total_nGComb=[];
sumA=[];
sumB=[];
% Get number of assesors for this test
DTF_Samples=length(files);
% Get some test variables and assign answer a numeric value
for i=1:DTF_Samples
    % Get one review file
    f=load(strcat(app.review_path,'\',files(i)));
    % Load file variables
    DTF_nComb=f.num_combs;
    DTF_nReps=f.num_replicates;
    DTF_nWavs=f.num_wavs;
    % Append file info to global arrays
    DTF_Combs=[DTF_Combs; f.comb];
    DTF_Results=[DTF_Results,f.answer];
    DTF_Times=[DTF_Times f.duration];
    % For each replication:
    for j=1:DTF_nReps
        % Find A and B results and assign 1 or 2, respectively
        temp(find(strcmp(f.answer(((j-1)*DTF_nComb)+1:j*DTF_nComb), 'A')))=1;
        temp(find(strcmp(f.answer(((j-1)*DTF_nComb)+1:j*DTF_nComb), 'B')))=2;
        % Create a matrix for all combs, from all assesors in all
        % replications
        DTF_Combs_Replicates(((i-1)*DTF_nComb)+1:i*DTF_nComb,1:3,j)=f.comb(((j-1)*DTF_nComb)+1:j*DTF_nComb,:);
        % Save results of every loop iteration
        DTF_Results_Replicates(((i-1)*DTF_nComb)+1:i*DTF_nComb,j)=temp;
        % Create a matrix with combs in columns 1 to 3 and results in
        % column 4
        DTF_Combs_Replicates(((i-1)*DTF_nComb)+1:i*DTF_nComb,4,j)=temp;
    end
end

% Compute number of correct answers for each replication
for i=1:DTF_nReps
    % Sort results like: 121, 122, 131, 133,...,1X1, 1XX,/here we are in the middle of the judgments/ 211, 212, 311, 313,...,X11, X1X
    DTF_Combs_Replicates_Sort(:,:,i)=sortrows(DTF_Combs_Replicates(:,:,i),[1,2,3]);
    counter=0;
    pass_comb=0;
    CorrectR_aux=[];
    EvaluatedRCombs_aux=[];
    TotalnRComb_aux=[];
    for j=1:size(DTF_Combs_Replicates_Sort,1)
        % Only execute j-for loop in new combs
        if(pass_comb<counter-1)
            pass_comb=pass_comb+1;
            continue;
        else
            pass_comb=0;
        end
        % How many ocurrences of each comb
        comb_ref=DTF_Combs_Replicates_Sort(j,1:3,i);
        counter=0;
        while(isequal(comb_ref,DTF_Combs_Replicates_Sort(j+counter,1:3,i)))
            counter=counter+1;
            if(j+counter>size(DTF_Combs_Replicates_Sort,1))
                break;
            end
        end
        % Get combs and total number of each comb
        EvaluatedRCombs_aux=[EvaluatedRCombs_aux;comb_ref];
        TotalnRComb_aux=[TotalnRComb_aux,counter];
        % Collect correct answers
        for k=2:3 % Visit A and B
            if(isequal(DTF_Combs_Replicates_Sort(j,k,i),DTF_Combs_Replicates_Sort(j,1,i))) % Find Ref in each comb
                if(~isequal(j,1))
                    size_aux=size(CorrectR_aux,2);
                else
                    size_aux=0;
                end
                switch k % Ref is A or B
                    case 2 % Ref=A(1)
                        CorrectR_aux(1,size_aux+1)=sum(DTF_Combs_Replicates_Sort(j:(j-1)+counter,4,i) == 1);
                    case 3 % Ref=B(2)
                        CorrectR_aux(1,size_aux+1)=sum(DTF_Combs_Replicates_Sort(j:(j-1)+counter,4,i) == 2);
                end
            end
        end
    end
    % Compose variables for app analysis
    Evaluated_RCombs=cat(3,Evaluated_RCombs,EvaluatedRCombs_aux);
    Correct_Replicates=[Correct_Replicates;CorrectR_aux];
    Total_nRComb=[Total_nRComb;TotalnRComb_aux];
end

% Mean elapsed time for tests (minutes:seconds)
DTF_avg_Time=seconds(mean(DTF_Times));
DTF_avg_Time.Format='mm:ss';

% Compute number of correct answers globally
temp_res(find(strcmp(DTF_Results,'A')))=1;
temp_res(find(strcmp(DTF_Results,'B')))=2;
DTF_Whole(:,1:3)=DTF_Combs;
DTF_Whole(:,4)=temp_res;
% Sort test results again, like previous coment, but globally this time
DTF_Combs_Sort=sortrows(DTF_Whole,[1,2,3]);
counter=0;
pass_comb=0;
for i=1:size(DTF_Combs_Sort,1)
    % Only execute i-for loop in new combs
    if(pass_comb<counter-1)
        pass_comb=pass_comb+1;
        continue;
    else
        pass_comb=0;
    end
    % How many ocurrences of each comb
    comb_ref=DTF_Combs_Sort(i,1:3);
    counter=0;
    while(isequal(comb_ref,DTF_Combs_Sort(i+counter,1:3)))
        counter=counter+1;
        if(i+counter>size(DTF_Combs_Sort,1))
            break;
        end
    end
    % Get combs and total number of each comb
    Evaluated_GCombs=[Evaluated_GCombs;comb_ref];
    Total_nGComb=[Total_nGComb,counter];
    % Collect correct answers
    for k=2:3 % Visit A and B
        if(isequal(DTF_Combs_Sort(i,k),DTF_Combs_Sort(i,1))) % Find Ref in each comb
            if(~isequal(i,1))
                size_aux=size(Correct,2);
            else
                size_aux=0;
            end
            switch k % Ref is A or B
                case 2 % Ref=A(1)
                    Correct(1,size_aux+1)=sum(DTF_Combs_Sort(i:(i-1)+counter,4) == 1);
                case 3 % Ref=B(2)
                    Correct(1,size_aux+1)=sum(DTF_Combs_Sort(i:(i-1)+counter,4) == 2);
            end
        end
    end
end
