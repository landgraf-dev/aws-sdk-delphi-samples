unit AWS.Transcribe.VocabularyFilter;

interface

uses
  Bcl.Types.Nullable,
  AWS.Transcribe;

type
  TVocabularyFilter = class (TVocabularyFilterInfo)
  private
    FDownloadUri: Nullable<string>;
    procedure SetDownloadUri(const AValue: string);
    function GetDownloadUri: string;
    function GetIsSetDownloadUri: Boolean;
  public
    property DownloadUri: string read GetDownloadUri write SetDownloadUri;
    property IsSetDownloadUri: Boolean read GetIsSetDownloadUri;
  end;

implementation

{ TVocabularyFilter }

function TVocabularyFilter.GetDownloadUri: string;
begin
  Result := FDownloadUri.ValueOrDefault;
end;

procedure TVocabularyFilter.SetDownloadUri(const AValue: string);
begin
  FDownloadUri := AValue;
end;

function TVocabularyFilter.GetIsSetDownloadUri: Boolean;
begin
  Result := FDownloadUri.HasValue;
end;

end.
