unit Forms.TranscriptionJob;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Buttons, AWS.Transcribe, AWS.Transcribe.Transcript;

type
  TTranscriptOption = (Raw, Segmented);

  TTranscriptionJobForm = class(TForm)
    PageControl: TPageControl;
    TranscriptTab: TTabSheet;
    TranscriptMemo: TMemo;
    ViewTranscriptOption: TRadioGroup;
    RedactedTranscriptTab: TTabSheet;
    RedactedTranscriptMemo: TMemo;
    ViewRedactedTranscriptOption: TRadioGroup;
    JobDetailsTab: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    JobNameEdit: TEdit;
    Label2: TLabel;
    LanguageCodeEdit: TComboBox;
    Label6: TLabel;
    MediaUriEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    VocabularyFilterEdit: TComboBox;
    Label5: TLabel;
    ContentRedactionEdit: TComboBox;
    ConfirmButton: TBitBtn;
    CancelButton: TBitBtn;
    procedure ViewRedactedTranscriptOptionClick(Sender: TObject);
    procedure ViewTranscriptOptionClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnEditChange(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FTranscriptionJob: TTranscriptionJob;
    FTranscript: TTranscript;
    FRedactedTranscript: TTranscript;
    function GetJobName: string;
    function GetLanguageCode: string;
    function GetMediaUri: string;
    function GetVocabularyFilter: string;
    function GetContentRedaction: string;
    procedure SetTranscript(const AValue: TTranscript);
    procedure SetRedactedTranscript(const AValue: TTranscript);
  private
    function InsertMode: Boolean;
    procedure ValidateForm;
    procedure FillFilterOprions;
    procedure FillLanguageOptions;
  private
    function GetTranscriptPayload(const AUri: string): string;
    procedure DisplayTranscript;
    procedure DisplayRedactedTranscript;
    procedure LoadTranscriptText(const ATranscript: TTranscript;
      const AText: TStrings; AOption: TTranscriptOption);
    property Transcript: TTranscript read FTranscript write SetTranscript;
    property RedactedTranscript: TTranscript read FRedactedTranscript write SetRedactedTranscript;
  public
    constructor Create(const ATranscriptionJob: TTranscriptionJob); reintroduce; overload;
    destructor Destroy; override;
    procedure SetTranscriptionJob(const ATranscriptionJob: TTranscriptionJob);
    procedure SetAvailableFilter(const AFilterName: string);
    property JobName: string read GetJobName;
    property LanguageCode: string read GetLanguageCode;
    property MediaUri: string read GetMediaUri;
    property VocabularyFilter: string read GetVocabularyFilter;
    property ContentRedaction: string read GetContentRedaction;
  end;

implementation

{$R *.dfm}

uses
  Sparkle.Http.Client;

const
  SAMPLE_MEDIA_URI = 'https://aws-ml-blog.s3.amazonaws.com/artifacts/transcribe_audio_processing/medical-diarization.wav';
  VALID_LANG_CODES = 'af-ZA,ar-AE,ar-SA,cy-GB,da-DK,de-CH,de-DE,en-AB,en-AU,en-GB,en-IE,en-IN,en-US,en-WL,es-ES,es-US,fa-IR,fr-CA,fr-FR,ga-IE,gd-GB,he-IL,hi-IN,id-ID,it-IT,ja-JP,ko-KR,ms-MY,nl-NL,pt-BR,pt-PT,ru-RU,ta-IN,te-IN,tr-TR,zh-CN,zh-TW,th-TH,en-ZA,en-NZ';

{ TTranscriptsForm }

constructor TTranscriptionJobForm.Create(const ATranscriptionJob: TTranscriptionJob);
begin
  inherited Create(nil);
  SetTranscriptionJob(ATranscriptionJob);
end;

destructor TTranscriptionJobForm.Destroy;
begin
  Transcript := nil;
  RedactedTranscript := nil;
  inherited;
end;

procedure TTranscriptionJobForm.FillLanguageOptions;
begin
  LanguageCodeEdit.Items.Clear;
  LanguageCodeEdit.Items.CommaText := VALID_LANG_CODES;
  LanguageCodeEdit.Items.Insert(0, 'Automatically identify language');
end;

procedure TTranscriptionJobForm.FillFilterOprions;
begin
  VocabularyFilterEdit.Items.Clear;
  VocabularyFilterEdit.Items.Add('None');
end;

procedure TTranscriptionJobForm.SetAvailableFilter(const AFilterName: string);
begin
  VocabularyFilterEdit.Items.Add(AFilterName);
end;

function TTranscriptionJobForm.GetJobName: string;
begin
  Result := Trim(JobNameEdit.Text);
end;

function TTranscriptionJobForm.GetLanguageCode: string;
begin
  if LanguageCodeEdit.ItemIndex>0 then
    Result := LanguageCodeEdit.Text
  else
    Result := ''; // identify language
end;

function TTranscriptionJobForm.GetMediaUri: string;
begin
  Result := MediaUriEdit.Text;
end;

function TTranscriptionJobForm.GetVocabularyFilter: string;
begin
  if VocabularyFilterEdit.ItemIndex>0 then
    Result := VocabularyFilterEdit.Text
  else
    Result := ''; // do not apply a vocabulary filter
end;

function TTranscriptionJobForm.GetContentRedaction: string;
begin
  case ContentRedactionEdit.ItemIndex of
    1: Result := TRedactionOutput.Redacted.Value;
    2: Result := TRedactionOutput.Redacted_and_unredacted.Value;
  else
    Result := ''; // do not apply content redaction
  end;
end;

function TTranscriptionJobForm.InsertMode: Boolean;
begin
  Result := FTranscriptionJob = nil;
end;

procedure TTranscriptionJobForm.OnEditChange(Sender: TObject);
begin
  ValidateForm;
end;

procedure TTranscriptionJobForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage=TranscriptTab then
  begin
    if TranscriptMemo.Lines.Text='' then
      DisplayTranscript;
  end else
  if PageControl.ActivePage=RedactedTranscriptTab then
  begin
    if RedactedTranscriptMemo.Lines.Text='' then
      DisplayRedactedTranscript;
  end;
end;

procedure TTranscriptionJobForm.ValidateForm;
begin
  ConfirmButton.Enabled := (JobName<>'') and (Pos(' ', JobName)=0) and (MediaUri<>'');
end;

procedure TTranscriptionJobForm.SetTranscriptionJob(const ATranscriptionJob: TTranscriptionJob);
var
  Json: string;
begin
  FTranscriptionJob := ATranscriptionJob;
  if ATranscriptionJob.Transcript.IsSetTranscriptFileUri then
  begin
    Json := GetTranscriptPayload(ATranscriptionJob.Transcript.TranscriptFileUri);
    Transcript := TTranscript.Create(Json);
  end else
    Transcript := nil;
  if ATranscriptionJob.Transcript.IsSetRedactedTranscriptFileUri then
  begin
    Json := GetTranscriptPayload(ATranscriptionJob.Transcript.RedactedTranscriptFileUri);
    RedactedTranscript := TTranscript.Create(Json);
  end else
    RedactedTranscript := nil;
end;

function TTranscriptionJobForm.GetTranscriptPayload(const AUri: string): string;
var
  CLient: THttpClient;
  Response: THttpResponse;
begin
  Client := THttpClient.Create;
  try
    Response := Client.Get(AUri);
    try
      Result := TEncoding.UTF8.GetString(Response.ContentAsBytes);
    finally
      Response.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure TTranscriptionJobForm.SetTranscript(const AValue: TTranscript);
begin
  if FTranscript<>nil then
    FTranscript.Free;
  FTranscript := AValue;
end;

procedure TTranscriptionJobForm.SetRedactedTranscript(const AValue: TTranscript);
begin
  if FRedactedTranscript<>nil then
    FRedactedTranscript.Free;
  FRedactedTranscript := AValue;
end;

procedure TTranscriptionJobForm.FormCreate(Sender: TObject);
begin
  TranscriptTab.TabVisible := False;
  RedactedTranscriptTab.TabVisible := False;
  FillLanguageOptions;
  FillFilterOprions;
end;

procedure TTranscriptionJobForm.FormShow(Sender: TObject);
begin
  ViewTranscriptOption.ItemIndex := 0;
  ViewRedactedTranscriptOption.ItemIndex := 0;
  if InsertMode then
  begin
    MediaUriEdit.Text := SAMPLE_MEDIA_URI;
    LanguageCodeEdit.ItemIndex := LanguageCodeEdit.Items.IndexOf(TLanguageCode.EnUS.Value); // since sample media is en-US
    VocabularyFilterEdit.ItemIndex := 0;
    ContentRedactionEdit.ItemIndex := 0;
  end else
  begin
    JobNameEdit.Text := FTranscriptionJob.TranscriptionJobName;
    if (FTranscriptionJob.IsSetIdentifyLanguage=False) or (FTranscriptionJob.IdentifyLanguage=False) then
    begin
      LanguageCodeEdit.Style := csDropDown;
      LanguageCodeEdit.Text := FTranscriptionJob.LanguageCode.Value;
    end else
      LanguageCodeEdit.ItemIndex := 0;
    MediaUriEdit.Text := FTranscriptionJob.Media.MediaFileUri;
    if FTranscriptionJob.Settings.IsSetVocabularyFilterName then
    begin
      VocabularyFilterEdit.Style := csDropDown;
      VocabularyFilterEdit.Text := FTranscriptionJob.Settings.VocabularyFilterName;
    end else
      VocabularyFilterEdit.ItemIndex := 0;
    if FTranscriptionJob.IsSetContentRedaction then
    begin
      if FTranscriptionJob.ContentRedaction.RedactionOutput=TRedactionOutput.Redacted then
        ContentRedactionEdit.ItemIndex := 1
      else
        ContentRedactionEdit.ItemIndex := 2;
    end else
      ContentRedactionEdit.ItemIndex := 0;
    if (FTranscriptionJob.TranscriptionJobStatus=TTranscriptionJobStatus.COMPLETED) and
       (FTranscriptionJob.IsSetTranscript) then
    begin
      TranscriptTab.TabVisible := (FTranscriptionJob.Transcript.IsSetTranscriptFileUri);
      RedactedTranscriptTab.TabVisible := (FTranscriptionJob.Transcript.IsSetRedactedTranscriptFileUri);
    end;
  end;
  PageControl.ActivePageIndex := 0;
  JobNameEdit.Enabled := InsertMode;
  LanguageCodeEdit.Enabled := InsertMode;
  MediaUriEdit.Enabled := InsertMode;
  ConfirmButton.Visible := InsertMode;
  VocabularyFilterEdit.Enabled := InsertMode;
  ContentRedactionEdit.Enabled := InsertMode;
end;

procedure TTranscriptionJobForm.DisplayTranscript;
begin
  if Transcript<>nil then
  begin
    TranscriptTab.TabVisible := True;
    LoadTranscriptText(Transcript, TranscriptMemo.Lines,
      TTranscriptOption(ViewTranscriptOption.ItemIndex));
  end else
    TranscriptTab.TabVisible := False;
end;

procedure TTranscriptionJobForm.DisplayRedactedTranscript;
begin
  if RedactedTranscript<>nil then
  begin
    RedactedTranscriptTab.TabVisible := True;
    LoadTranscriptText(RedactedTranscript, RedactedTranscriptMemo.Lines,
      TTranscriptOption(ViewRedactedTranscriptOption.ItemIndex));
  end else
    RedactedTranscriptTab.TabVisible := False;
end;

procedure TTranscriptionJobForm.ViewTranscriptOptionClick(Sender: TObject);
begin
  if Transcript<>nil then
    DisplayTranscript;
end;

procedure TTranscriptionJobForm.ViewRedactedTranscriptOptionClick(Sender: TObject);
begin
  if RedactedTranscript<>nil then
    DisplayRedactedTranscript;
end;

procedure TTranscriptionJobForm.LoadTranscriptText(const ATranscript: TTranscript;
  const AText: TStrings; AOption: TTranscriptOption);
var
  I: Integer;
  S: string;
  TranscriptItem: TTranscriptItem;
begin
  AText.Clear;
  if ATranscript<>nil then
  begin
    if (AOption=TTranscriptOption.Segmented) and (ATranscript.SegmentCount>1) then
    begin
      for I := 0 to ATranscript.SegmentCount-1 do
      begin
        S := Format('[%s]', [ATranscript.Segments[I].SpeakerLabel]);
        for TranscriptItem in ATranscript.Segments[I].Items do
        begin
          if TranscriptItem.ItemType=TTrancriptType.pronunciation then
            S := S + ' ';
          S := S + TranscriptItem.Alternatives[0].Content; // use the first alternative
        end;
        AText.Add(S);
        AText.Add('');
      end;
    end else
    begin
      for I := 0 to ATranscript.TextCount-1 do
      begin
        AText.Add(ATranscript[I]);
        AText.Add('');
      end;
    end;
    AText.Delete(AText.Count-1);
  end;
end;

end.
