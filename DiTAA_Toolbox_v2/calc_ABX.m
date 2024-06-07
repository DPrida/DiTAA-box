% Obtain all review (.mat files) names in selected directory is made
% in .mlapp, app and script share workspace so 'files' is defined
% previously

% Variables preallocate
ABX_Results=[];
ABX_Results_Replicates=[];
ABX_Combs=[];
ABX_Times=[];
Correct_Replicates=[];
Evaluated_RCombs=[];
Total_nRComb=[];
Correct=[];
Evaluated_GCombs=[];
Total_nGComb=[];
sumA=[];
sumB=[];
% Get number of assesors for this test
ABX_Samples=length(files);
% Get some test variables and assign answer a numeric value
for i=1:ABX_Samples
    % Get one review file
    f=load(strcat(app.review_path,'\',files(i)));
    % Load file variables
    ABX_nComb=f.num_combs;
    ABX_nSeq=f.seq_value;
    ABX_nReps=f.num_replicates;
    ABX_nWavs=f.num_wavs;
    % Append file info to global arrays
    ABX_Combs=[ABX_Combs;f.comb];
    ABX_Results=[ABX_Results,f.answer];
    ABX_Times=[ABX_Times, f.duration];
    % For each replication:
    for j=1:ABX_nReps
        % Find A and B results and assign 1 or 2, respectively
        temp(find(strcmp(f.answer(((j-1)*ABX_nComb)+1:j*ABX_nComb),'A')))=1;
        temp(find(strcmp(f.answer(((j-1)*ABX_nComb)+1:j*ABX_nComb),'B')))=2;
        % Create a matrix for all combs, from all assesors in all
        % replications
        ABX_Combs_Replicates(((i-1)*ABX_nComb)+1:i*ABX_nComb,1:3,j)=f.comb(((j-1)*ABX_nComb)+1:j*ABX_nComb,:);
        % Save results of every loop iteration
        ABX_Results_Replicates(((i-1)*ABX_nComb)+1:i*ABX_nComb,j)=temp';
        % Create a matrix with combs in columns 1 to 3 and results in
        % column 4
        ABX_Combs_Replicates(((i-1)*ABX_nComb)+1:i*ABX_nComb,4,j)=temp';
    end
end

% Compute number of correct answers for each replication
for i=1:ABX_nReps
    % Sort results like: 121, 122, 131, 133,...,1X1, 1XX,/here we are in the middle of the judgments/ 211, 212, 311, 313,...,X11, X1X
    ABX_Combs_Replicates_Sort(:,:,i)=sortrows(ABX_Combs_Replicates(:,:,i),[1,2,3]);
    counter=0;
    pass_comb=0;
    CorrectR_aux=[];
    EvaluatedRCombs_aux=[];
    TotalnRComb_aux=[];
    for j=1:size(ABX_Combs_Replicates_Sort,1)
        % Only execute j-for loop in new combs
        if(pass_comb<counter-1)
            pass_comb=pass_comb+1;
            continue;
        else
            pass_comb=0;
        end
        % How many ocurrences of each comb
        comb_ref=ABX_Combs_Replicates_Sort(j,1:3,i);
        counter=0;
        while(isequal(comb_ref,ABX_Combs_Replicates_Sort(j+counter,1:3,i)))
            counter=counter+1;
            if(j+counter>size(ABX_Combs_Replicates_Sort,1))
                break;
            end
        end
        % Get combs and total number of each comb
        EvaluatedRCombs_aux=[EvaluatedRCombs_aux;comb_ref];
        TotalnRComb_aux=[TotalnRComb_aux,counter];
        % Collect correct answers
        for k=1:2 % Visit A and B
            if(isequal(ABX_Combs_Replicates_Sort(j,k,i),ABX_Combs_Replicates_Sort(j,3,i))) % Find X in each comb
                if(~isequal(j,1))
                    size_aux=size(CorrectR_aux,2);
                else
                    size_aux=0;
                end
                switch k % X is A or B
                    case 1 % X=A(1)
                        CorrectR_aux(1,size_aux+1)=sum(ABX_Combs_Replicates_Sort(j:(j-1)+counter,4,i) == 1);
                    case 2 % X=B(2)
                        CorrectR_aux(1,size_aux+1)=sum(ABX_Combs_Replicates_Sort(j:(j-1)+counter,4,i) == 2);
                end
            end
        end
    end
    % Compose variables for app analysis
    Evaluated_RCombs=cat(3,Evaluated_RCombs,EvaluatedRCombs_aux);
    Correct_Replicates=[Correct_Replicates;CorrectR_aux];
    Total_nRComb=[Total_nRComb;TotalnRComb_aux];
end

% Mean elapsed time for tests (minutes.seconds)
ABX_avg_Time=seconds(mean(ABX_Times));
ABX_avg_Time.Format='mm:ss';

% Compute number of correct answers globally
temp_res(find(strcmp(ABX_Results,'A')))=1;
temp_res(find(strcmp(ABX_Results,'B')))=2;
ABX_Whole(:,1:3)=ABX_Combs;
ABX_Whole(:,4)=temp_res;
% Sort test results again, like previous coment, but globally this time
ABX_Combs_Sort=sortrows(ABX_Whole,[1,2,3]);
counter=0;
pass_comb=0;
for i=1:size(ABX_Combs_Sort,1)
    % Only execute i-for loop in new combs
    if(pass_comb<counter-1)
        pass_comb=pass_comb+1;
        continue;
    else
        pass_comb=0;
    end
    % How many ocurrences of each comb
    comb_ref=ABX_Combs_Sort(i,1:3);
    counter=0;
    while(isequal(comb_ref,ABX_Combs_Sort(i+counter,1:3)))
        counter=counter+1;
        if(i+counter>size(ABX_Combs_Sort,1))
            break;
        end
    end
    % Get combs and total number of each comb
    Evaluated_GCombs=[Evaluated_GCombs;comb_ref];
    Total_nGComb=[Total_nGComb,counter];
    % Collect correct answers
    for k=1:2 % Visit A and B
        if(isequal(ABX_Combs_Sort(i,k),ABX_Combs_Sort(i,3))) % Find X in each comb
            if(~isequal(i,1))
                size_aux=size(Correct,2);
            else
                size_aux=0;
            end
            switch k % X is A or B
                case 1 % X=A(1)
                    Correct(1,size_aux+1)=sum(ABX_Combs_Sort(i:(i-1)+counter,4) == 1);
                case 2 % X=B(2)
                    Correct(1,size_aux+1)=sum(ABX_Combs_Sort(i:(i-1)+counter,4) == 2);
            end
        end
    end
end

% d' calculations
HA_global=sum(sumA)/(2*(ABX_nWavs-1)*(ABX_nReps*ABX_Samples));
HB_global=sum(sumB)/(2*(ABX_nWavs-1)*(ABX_nReps*ABX_Samples));

FA_global=1-HB_global;
FB_global=1-HA_global;


zHa_global=norminv(HA_global);
zHb_global=norminv(HB_global);
zFa_global=norminv(FA_global);
zFb_global=norminv(FB_global);

resta_a_global=zHa_global-zFa_global;
resta_b_global=zHb_global-zFb_global;
