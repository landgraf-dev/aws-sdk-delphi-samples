unit Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Generics.Collections, AWS.Transcribe, AWS.Transcribe.Transcript,
  AWS.Transcribe.VocabularyFilter;

type
  TJobStatus = (COMPLETED, FAILED, IN_PROGRESS, QUEUED);

  TJobParams = record
    JobName: string;
    LanguageCode: string;
    MediaUri: string;
    VocabularyFilter: string;
    ContentRedaction: string;
  end;

  TFilterParams = record
    FilterName: string;
    LanguageCode: string;
    Words: TArray<string>;
  end;

  TMainForm = class(TForm)
    PageControl: TPageControl;
    JobsTab: TTabSheet;
    FiltersTab: TTabSheet;
    Panel2: TPanel;
    Label2: TLabel;
    ListJobsButton: TSpeedButton;
    DeleteJobButton: TSpeedButton;
    ViewJobButton: TSpeedButton;
    CreateJobButton: TSpeedButton;
    JobsView: TListView;
    Panel3: TPanel;
    Label3: TLabel;
    ListFiltersButton: TSpeedButton;
    DeleteFilterButton: TSpeedButton;
    ViewFilterButton: TSpeedButton;
    CreateFilterButton: TSpeedButton;
    FiltersView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListFiltersButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CreateFilterButtonClick(Sender: TObject);
    procedure ListJobsButtonClick(Sender: TObject);
    procedure CreateJobButtonClick(Sender: TObject);
    procedure DeleteJobButtonClick(Sender: TObject);
    procedure DeleteFilterButtonClick(Sender: TObject);
    procedure ViewJobButtonClick(Sender: TObject);
    procedure JobsViewData(Sender: TObject; Item: TListItem);
    procedure JobsViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure JobsViewCustomDrawSubItem(Sender: TCustomListView; Item: TListItem;
      SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ViewFilterButtonClick(Sender: TObject);
    procedure FiltersViewData(Sender: TObject; Item: TListItem);
    procedure PageControlChange(Sender: TObject);
    procedure FiltersViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  private
    FJobs: TObjectList<TTranscriptionJobSummary>;
    FFilters: TObjectList<TVocabularyFilterInfo>;
    FLoadedFilters: Boolean;
    procedure SetJobs(const AValue: TObjectList<TTranscriptionJobSummary>);
    procedure SetFilters(const AValue: TObjectList<TVocabularyFilterInfo>);
    function GetSelectedJob: TTranscriptionJobSummary;
    function GetSelectedFilter: TVocabularyFilterInfo;
  private
    function JobStatus(const AValue: string): TJobStatus;
    function BuildTranscriptionJobRequest(const AParams: TJobParams): IStartTranscriptionJobRequest;
  public
    procedure CreateTrancriptionJob(const AParams: TJobParams);
    procedure ListTrancriptionsJobs;
    function GetTranscriptionJob(const AName: string): TTranscriptionJob;
    procedure DeleteTrancriptionJob(const AName: string);
    procedure CreateVocabularyFilter(const AParams: TFilterParams);
    procedure ListVocabularyFilters;
    function GetVocabularyFilter(const AName: string): TVocabularyFilter;
    procedure DeleteVocabularyFilter(const AName: string);
  public
    property Jobs: TObjectList<TTranscriptionJobSummary> read FJobs write SetJobs;
    property SelectedJob: TTranscriptionJobSummary read GetSelectedJob;
    property Filters: TObjectList<TVocabularyFilterInfo> read FFilters write SetFilters;
    property SelectedFilter: TVocabularyFilterInfo read GetSelectedFilter;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.TypInfo,
  AWS.Internal.WebResponseData,
  Forms.TranscriptionJob,
  Forms.VocabularyFilter;

{$R *.dfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FJobs := TObjectList<TTranscriptionJobSummary>.Create(True);
  FFilters := TObjectList<TVocabularyFilterInfo>.Create(True);
  FLoadedFilters := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Filters := nil;
  Jobs := nil;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PageControl.ActivePageIndex := 0;
  ListJobsButton.Click;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  if (PageControl.ActivePage=FiltersTab) and (not FLoadedFilters) then
  begin
    FLoadedFilters := True;
    ListVocabularyFilters;
  end;
end;

{ Transcription Jobs Tab }

procedure TMainForm.ListJobsButtonClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    ListTrancriptionsJobs;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.CreateJobButtonClick(Sender: TObject);
var
  I: Integer;
  JobParams: TJobParams;
begin
  { get available filters }
  if FFilters.Count=0 then
    ListFiltersButton.Click;
  { create job }
  with TTranscriptionJobForm.Create(Self) do
  try
    for I := 0 to FFilters.Count-1 do
      SetAvailableFilter(FFilters[I].VocabularyFilterName);
    if ShowModal=mrOk then
    begin
      Screen.Cursor := crHourGlass;
      try
        JobParams.JobName := JobName;
        JobParams.LanguageCode := LanguageCode;
        JobParams.MediaUri := MediaUri;
        JobParams.VocabularyFilter := VocabularyFilter;
        JobParams.ContentRedaction := ContentRedaction;
        CreateTrancriptionJob(JobParams);
        ListTrancriptionsJobs;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.ViewJobButtonClick(Sender: TObject);
var
  Job: TTranscriptionJob;
begin
  if (SelectedJob<>nil) {and (SelectedJob.TranscriptionJobStatus=TTranscriptionJobStatus.COMPLETED)} then
  begin
    Screen.Cursor := crHourGlass;
    try
      Job := GetTranscriptionJob(SelectedJob.TranscriptionJobName);
    finally
      Screen.Cursor := crDefault;
    end;
    with TTranscriptionJobForm.Create(Job) do
    try
      ShowModal;
    finally
      Job.Free;
      Free;
    end;
  end else
    MessageDlg('Select a Job to view details', TMsgDlgType.mtInformation, [mbOK], 0);
end;

procedure TMainForm.DeleteJobButtonClick(Sender: TObject);
begin
  if SelectedJob<>nil then
  begin
    if MessageDlg(Format('Delete job %s?', [SelectedJob.TranscriptionJobName]),
      TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
    try
      Screen.Cursor := crHourGlass;
      DeleteTrancriptionJob(SelectedJob.TranscriptionJobName);
      ListTrancriptionsJobs;
    finally
      Screen.Cursor := crDefault;
    end;
  end else
    MessageDlg('Select a Job to delete', TMsgDlgType.mtInformation, [mbOK], 0);
end;

procedure TMainForm.JobsViewData(Sender: TObject; Item: TListItem);
var
  lJob: TTranscriptionJobSummary;
begin
  lJob := Jobs[Item.Index];
  Item.Caption := lJob.TranscriptionJobName;
  Item.SubItems.Add(lJob.LanguageCode.Value);
  Item.SubItems.Add(DateTimeToStr(lJob.CreationTime));
  Item.SubItems.Add(lJob.TranscriptionJobStatus.Value);
end;

procedure TMainForm.JobsViewCustomDrawSubItem(Sender: TCustomListView; Item: TListItem;
  SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  lStatus: TJobStatus;
  lFColor: TColor;
begin
  DefaultDraw := True;
  lFColor := clBlack;
  if (SubItem=3) then // status column
  begin
    lStatus := JobStatus(Jobs[Item.Index].TranscriptionJobStatus.Value);
    case lStatus of
      QUEUED: lFColor := clGray;
      IN_PROGRESS: lFColor := clBlue;
      COMPLETED: lFColor := clGreen;
      FAILED: lFColor := clRed;
    end;
  end;
  Sender.Canvas.Font.Color := lFColor;
end;

procedure TMainForm.JobsViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  DeleteJobButton.Enabled := (JobsView.ItemIndex>=0);
  ViewJobButton.Enabled := (JobsView.ItemIndex>=0)
    and (Jobs[JobsView.ItemIndex].TranscriptionJobStatus = TTranscriptionJobStatus.COMPLETED);
end;

{ Vocabulary Filters Tab }

procedure TMainForm.ListFiltersButtonClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    ListVocabularyFilters;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.CreateFilterButtonClick(Sender: TObject);
var
  FilterParams: TFilterParams;
begin
  with TVocabularyFilterForm.Create(Self) do
  try
    if ShowModal=mrOk then
    begin
      Screen.Cursor := crHourGlass;
      try
        FilterParams.FilterName := FilterName;
        FilterParams.LanguageCode := LanguageCode;
        FilterParams.Words := Words;
        CreateVocabularyFilter(FilterParams);
        ListVocabularyFilters;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.ViewFilterButtonClick(Sender: TObject);
var
  Filter: TVocabularyFilter;
begin
  if (SelectedFilter<>nil) then
  begin
    Screen.Cursor := crHourGlass;
    try
      Filter := GetVocabularyFilter(SelectedFilter.VocabularyFilterName);
    finally
      Screen.Cursor := crDefault;
    end;
    with TVocabularyFilterForm.Create(Filter) do
    try
      ShowModal;
    finally
      Filter.Free;
      Free;
    end;
  end else
    MessageDlg('Select a Filter to view details', TMsgDlgType.mtInformation, [mbOK], 0);
end;

procedure TMainForm.DeleteFilterButtonClick(Sender: TObject);
begin
  if SelectedFilter<>nil then
  begin
    if MessageDlg(Format('Delete filter %s?', [SelectedFilter.VocabularyFilterName]),
      TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
    try
      Screen.Cursor := crHourGlass;
      DeleteVocabularyFilter(SelectedFilter.VocabularyFilterName);
      ListVocabularyFilters;
    finally
      Screen.Cursor := crDefault;
    end;
  end else
    MessageDlg('Select a Filter to delete', TMsgDlgType.mtInformation, [mbOK], 0);
end;

procedure TMainForm.FiltersViewData(Sender: TObject; Item: TListItem);
var
  lFilter: TVocabularyFilterInfo;
begin
  lFilter := Filters[Item.Index];
  Item.Caption := lFilter.VocabularyFilterName;
  Item.SubItems.Add(lFilter.LanguageCode.Value);
  Item.SubItems.Add(DateTimeToStr(lFilter.LastModifiedTime));
end;

procedure TMainForm.FiltersViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  DeleteFilterButton.Enabled := (FiltersView.ItemIndex>=0);
  ViewFilterButton.Enabled := (FiltersView.ItemIndex>=0);
end;

{ Getters and Setters }

function TMainForm.GetSelectedJob: TTranscriptionJobSummary;
begin
  if (JobsView.Items.Count>0) and (JobsView.ItemIndex>=0) then
    Result := Jobs[JobsView.ItemIndex]
  else
    Result := nil;
end;

function TMainForm.GetSelectedFilter: TVocabularyFilterInfo;
begin
  if (FiltersView.Items.Count>0) and (FiltersView.ItemIndex>=0) then
    Result := Filters[FiltersView.ItemIndex]
  else
    Result := nil;
end;

procedure TMainForm.SetJobs(const AValue: TObjectList<TTranscriptionJobSummary>);
begin
  if FJobs<>nil then
    FJobs.Free;
  FJobs := AValue;
  if FJobs<>nil then
  begin
    JobsView.Items.Count := FJobs.Count;
    JobsView.Invalidate;
  end else
    JobsView.Items.Count := 0;
end;

procedure TMainForm.SetFilters(const AValue: TObjectList<TVocabularyFilterInfo>);
begin
  if FFilters<>nil then
    FFilters.Free;
  FFilters := AValue;
  if FFilters<>nil then
  begin
    FiltersView.Items.Count := FFilters.Count;
    FiltersView.Invalidate;
  end else
    FiltersView.Items.Count := 0;
end;

{ helper functions }

function TMainForm.JobStatus(const AValue: string): TJobStatus;
begin
  Result := TJobStatus(GetEnumValue(TypeInfo(TJobStatus), AValue));
end;

{ AWS actions for Jobs }

procedure TMainForm.ListTrancriptionsJobs;
var
  Client: IAmazonTranscribeService;
  Request: IListTranscriptionJobsRequest;
  Response: IListTranscriptionJobsResponse;
begin
  // Create client
  Client := TAmazonTranscribeServiceClient.Create;

  // Setup request
  Request := TListTranscriptionJobsRequest.Create;
  Request.MaxResults := 20;

  // Execute
  Response := Client.ListTranscriptionJobs(Request);

  if Response.HttpStatusCode=200 then
  begin
    Response.KeepTranscriptionJobSummaries := True;
    Jobs := Response.TranscriptionJobSummaries;
  end;
end;

procedure TMainForm.CreateTrancriptionJob(const AParams: TJobParams);
var
  Client: IAmazonTranscribeService;
  Request: IStartTranscriptionJobRequest;
  Response: IStartTranscriptionJobResponse;
begin
  // Create client
  Client := TAmazonTranscribeServiceClient.Create;

  // Setup request
  Request := BuildTranscriptionJobRequest(AParams);

  // Execute
  Response := Client.StartTranscriptionJob(Request);
end;

function TMainForm.BuildTranscriptionJobRequest(const AParams: TJobParams): IStartTranscriptionJobRequest;

  function GetMediaFormat(const AUri: string): string;
  begin
    Result := StringReplace(ExtractFileExt(AUri), '.', '', []);
  end;

var
  Media: TMedia;
  Redaction: TContentRedaction;
  Settings: TSettings;
begin
  Media := TMedia.Create;
  Media.MediaFileUri := AParams.MediaUri;

  Redaction := nil;
  if AParams.ContentRedaction<>'' then
  begin
    Redaction := TContentRedaction.Create;
    with Redaction do
    begin
      RedactionType := TRedactionType.PII;
      RedactionOutput := AParams.ContentRedaction;
      PiiEntityTypes := TList<string>.Create(['NAME']); // suppress people's names
    end;
  end;

  Settings := TSettings.Create;
  with Settings do
  begin
    ShowAlternatives := True;
    MaxAlternatives := 2;
    ShowSpeakerLabels := True;
    MaxSpeakerLabels := 2;
    if AParams.VocabularyFilter<>'' then
    begin
      VocabularyFilterMethod := TVocabularyFilterMethod.Mask; // mask filtered words
      VocabularyFilterName := AParams.VocabularyFilter;
    end;
  end;

  Result := TStartTranscriptionJobRequest.Create;
  Result.TranscriptionJobName := AParams.JobName; { required }
  if AParams.LanguageCode<>'' then
    Result.LanguageCode := AParams.LanguageCode { eg. TLanguageCode.EnUS }
  else
    Result.IdentifyLanguage := True; { required: LanguageCode or IdentifyLanguage }
  Result.Media := Media; { required }
  Result.MediaFormat := GetMediaFormat(AParams.MediaUri); { not required; eg. TMediaFormat.Wav }
  Result.Settings := Settings;
  if AParams.ContentRedaction<>'' then
    Result.ContentRedaction := Redaction;
end;

function TMainForm.GetTranscriptionJob(const AName: string): TTranscriptionJob;
var
  Client: IAmazonTranscribeService;
  Request: IGetTranscriptionJobRequest;
  Response: IGetTranscriptionJobResponse;
begin
  // Create client
  Client := TAmazonTranscribeServiceClient.Create;

  // Setup request
  Request := TGetTranscriptionJobRequest.Create;
  Request.TranscriptionJobName := AName;

  // Execute
  Response := Client.GetTranscriptionJob(Request);

  if (Response.HttpStatusCode=200) then
  begin
    Result := Response.TranscriptionJob;
    Response.KeepTranscriptionJob := True;
  end else
    Result := nil;

end;

procedure TMainForm.DeleteTrancriptionJob(const AName: string);
var
  Client: IAmazonTranscribeService;
  Request: IDeleteTranscriptionJobRequest;
  Response: IDeleteTranscriptionJobResponse;
begin
  // Create client
  Client := TAmazonTranscribeServiceClient.Create;

  // Setup request
  Request := TDeleteTranscriptionJobRequest.Create;
  Request.TranscriptionJobName := AName;

  // Execute
  Response := Client.DeleteTranscriptionJob(Request);
  if Response.HttpStatusCode=200 then
    ShowMessage('Job successfully deleted');
end;

{ AWS actions for Filters }

procedure TMainForm.ListVocabularyFilters;
var
  Client: IAmazonTranscribeService;
  Request: IListVocabularyFiltersRequest;
  Response: IListVocabularyFiltersResponse;
begin
  // Create client
  Client := TAmazonTranscribeServiceClient.Create;

  // Setup request
  Request := TListVocabularyFiltersRequest.Create;

  // Execute
  Response := Client.ListVocabularyFilters(Request);

  if Response.HttpStatusCode=200 then
  begin
    Response.KeepVocabularyFilters := True;
    Filters := Response.VocabularyFilters;
  end;
end;

procedure TMainForm.CreateVocabularyFilter(const AParams: TFilterParams);
var
  Client: IAmazonTranscribeService;
  Request: ICreateVocabularyFilterRequest;
  Response: ICreateVocabularyFilterResponse;
begin
  // Create client
  Client := TAmazonTranscribeServiceClient.Create;

  // Setup request
  Request := TCreateVocabularyFilterRequest.Create;
  Request.VocabularyFilterName := AParams.FilterName;
  Request.LanguageCode := AParams.LanguageCode;
  Request.Words := TList<string>.Create(AParams.Words); // some words to the vocabulary filter

  try
    Response := Client.CreateVocabularyFilter(Request);
    if Response.HttpStatusCode=200 then
      ShowMessage(Format('Vocabulary filter "%s" successfully created.', [AParams.FilterName]));
  except
    on E: EHttpErrorResponseException do
      ShowMessage(E.Message); // do something with E
  end;
end;

function TMainForm.GetVocabularyFilter(const AName: string): TVocabularyFilter;
var
  Client: IAmazonTranscribeService;
  Request: IGetVocabularyFilterRequest;
  Response: IGetVocabularyFilterResponse;
begin
  // Create client
  Client := TAmazonTranscribeServiceClient.Create;

  // Setup request
  Request := TGetVocabularyFilterRequest.Create;
  Request.VocabularyFilterName := AName;

  // Execute
  Response := Client.GetVocabularyFilter(Request);

  if (Response.HttpStatusCode=200) then
  begin
    Result := TVocabularyFilter.Create;
    if Response.IsSetDownloadUri then
      Result.DownloadUri := Response.DownloadUri;
    if Response.IsSetLanguageCode then
      Result.LanguageCode := Response.LanguageCode;
    if Response.IsSetLastModifiedTime then
      Result.LastModifiedTime := Response.LastModifiedTime;
    if Response.IsSetVocabularyFilterName then
      Result.VocabularyFilterName := Response.VocabularyFilterName;
  end else
    Result := nil;

end;

procedure TMainForm.DeleteVocabularyFilter(const AName: string);
var
  Client: IAmazonTranscribeService;
  Request: IDeleteVocabularyFilterRequest;
  Response: IDeleteVocabularyFilterResponse;
begin
  // Create client
  Client := TAmazonTranscribeServiceClient.Create;

  // Setup request
  Request := TDeleteVocabularyFilterRequest.Create;
  Request.VocabularyFilterName := AName;

  // Execute
  Response := Client.DeleteVocabularyFilter(Request);
  if Response.HttpStatusCode=200 then
    ShowMessage('Filter successfully deleted');
end;

end.

