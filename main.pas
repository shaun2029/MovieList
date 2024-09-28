unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Types, ssl_openssl, httpsend, synacode, fpjson, jsonparser, Process, Keys, moviedb;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    btnLoadMovieList: TButton;
    imgPoster: TImage;
    lbMovieInfo: TLabel;
    lbxMovies: TListBox;
    dlgOpenFile: TOpenDialog;
    mmMovieInfo: TMemo;
    dlgOpenDirectory: TSelectDirectoryDialog;
    procedure btnLoadMovieListClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxMoviesClick(Sender: TObject);
    procedure lbxMoviesDblClick(Sender: TObject);
    procedure lbxMoviesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbxMoviesKeyPress(Sender: TObject; var Key: char);
  private
    FMovieDBFileName: string;
    FMovieDB : TMovieDB;

    function DownloadImage(const URL: string; Stream: TMemoryStream): boolean;
    function EndsWithYear(var Title: string; out Year: Integer): Boolean;
    function ExtractFilePathFromLocation(const LocationStr: string): string;
    function ExtractMovieInfo(const JSONString: string): TMovieInfo;
    function ExtractRatingsAsList(const JSONString: string): String;
    function ExtractYearFromParentheses(var Input: string): Integer;
    function GetCachedMovieInfo(const Filename, FilenameTitle: string; Year: integer): TMovieInfo;
    function GetFilename(Filename: string): string;
    function GetFilenameTitle(Filename: string): string;
    function GetMovieInfo(Title: string; Year: integer; out Found: boolean): string;
    procedure GetVideoFiles(const Folder: string; VideoFiles: TStrings);
    function IsTitle(Index: Integer): boolean;
    function IsYear(const Str: string; out Year: Integer): Boolean;
    procedure LoadAndDisplayImage(const URL: string);
    procedure MovieToggle(Sender: TObject);
    procedure PlayMovie(const MovieFile: string);
    procedure UpdateMovieInfo(Index: Integer);

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnLoadMovieListClick(Sender: TObject);
var
  Filename: string;
begin
  if dlgOpenDirectory.Execute then
  begin
    lbxMovies.Clear;
    GetVideoFiles(dlgOpenDirectory.FileName, lbxMovies.Items);
    lbxMovies.Sorted := True;
    lbxMovies.Items.Insert(0, 'File Location: ' + dlgOpenDirectory.FileName);
    Filename := GetAppConfigDir(False) + 'MovieFileList.txt';
    lbxMovies.Items.SaveToFile(Filename)
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  CacheFileName: string;
begin
  FMovieDBFileName := GetAppConfigDir(False) + 'MovieInfo.db';
  FMovieDB := TMovieDB.Create(FMovieDBFileName);
end;

procedure TfrmMain.FormDblClick(Sender: TObject);
begin
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FMovieDB.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  Filename: string;
  ConfigDir: string;
begin
  // Get the application config directory path
  ConfigDir := GetAppConfigDir(False);

  // Ensure the directory exists, create it if not
  if not DirectoryExists(ConfigDir) then
    ForceDirectories(ConfigDir);

  Filename := ConfigDir + 'MovieFileList.txt';

  if FileExists(Filename) then
    lbxMovies.Items.LoadFromFile(Filename);
end;

procedure TfrmMain.lbxMoviesClick(Sender: TObject);
begin
  if lbxMovies.ItemIndex <> -1 then
  begin
    UpdateMovieInfo(lbxMovies.ItemIndex);
  end;
end;

procedure TfrmMain.lbxMoviesDblClick(Sender: TObject);
var
  Filename: String;
begin
  if (lbxMovies.ItemIndex >= 0) and not IsTitle(lbxMovies.ItemIndex) then
  begin
    try
      Filename := ExtractFilePathFromLocation(lbxMovies.Items[0]) +  lbxMovies.Items[lbxMovies.ItemIndex];
      PlayMovie(Filename)
    except
      on E: Exception do
      begin
        // Show the exception message in a dialog
        ShowMessage('Error: ' + E.Message);
      end;
    end;
  end;
end;

procedure TfrmMain.lbxMoviesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  MovieTitle: string;
begin
  with lbxMovies.Canvas do
  begin
    MovieTitle := lbxMovies.Items.Strings[Index];

    if Trim(MovieTitle) <> '' then
    begin
      Font.Style := [];
      if MovieTitle[1] = '-' then Font.Style := [fsStrikeOut]
      else if MovieTitle[1] = '?' then Font.Style := [fsUnderline, fsItalic];
    end;

    // Clear the background
    FillRect(ARect);

    // Draw the text with the selected font style
    TextOut(ARect.Left + 2, ARect.Top, MovieTitle);
  end;
end;

function TfrmMain.IsTitle(Index: Integer): boolean;
begin
  if (lbxMovies.ItemIndex >= 0) then
    Result := (Pos('File Location:', lbxMovies.Items.Strings[Index]) = 1)
  else Result := False;
end;

function TfrmMain.ExtractFilePathFromLocation(const LocationStr: string): string;
const
  Prefix = 'File Location: ';
begin
  // Check if the string starts with the expected prefix
  if Pos(Prefix, LocationStr) = 1 then
  begin
    // Extract the path by removing the prefix
    Result := Copy(LocationStr, Length(Prefix) + 1, Length(LocationStr) - Length(Prefix));

    // Add directory separator if not present
    if (Result <> '') and (Result[Length(Result)] <> DirectorySeparator) then
    begin
      Result := Result + DirectorySeparator;
    end;
  end
  else
  begin
    // Return an empty string if the prefix doesn't match
    Result := '';
  end;
end;

function TfrmMain.GetFilename(Filename:string): string;
begin
  Result := Filename;
  if (Result[1] = '-') or (Result[1] = '?') then
    Result := Copy(Result, 2, Length(Result) - 1);
end;

function TfrmMain.GetFilenameTitle(Filename:string): string;
begin
  Result := ChangeFileExt(Result, '');
end;

procedure TfrmMain.UpdateMovieInfo(Index: Integer);
var
  Filename, FilenameTitle: string;
  MovieInfo: TMovieInfo;
  Year: integer;
begin
  mmMovieInfo.Clear;
  imgPoster.Picture.Clear;
  lbMovieInfo.Caption := 'Movie Info:';

  if (lbxMovies.ItemIndex >= 0) and not IsTitle(Index) then
  begin
    Filename := GetFilename(lbxMovies.Items.Strings[Index]);
    FilenameTitle := GetFilenameTitle(Filename);

    Year := ExtractYearFromParentheses(FilenameTitle);
    if Year < 1900 then EndsWithYear(FilenameTitle, Year);

    MovieInfo := GetCachedMovieInfo(Filename, FilenameTitle, Year);
    lbMovieInfo.Caption := MovieInfo.Title;
    mmMovieInfo.Lines.Add('Actors: ' + MovieInfo.Actors);
    mmMovieInfo.Lines.Add(Format('Year: %d  Genre: %s', [MovieInfo.Year, MovieInfo.Genre, MovieInfo.Ratings]));
    mmMovieInfo.Lines.Add('Ratings:' + LineEnding + ExtractRatingsAsList(MovieInfo.Ratings));
    mmMovieInfo.Lines.Add('Plot: ' + MovieInfo.Plot);

    if MovieInfo.Poster <> '' then
    begin
      LoadAndDisplayImage(MovieInfo.Poster);
    end;
  end;
end;

procedure TfrmMain.lbxMoviesKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = ' ') or (Key = #13) then
    MovieToggle(Self);
end;

procedure TfrmMain.MovieToggle(Sender: TObject);
var
  MovieTitle, Filename: string;
begin
  if (lbxMovies.ItemIndex >= 0) and not IsTitle(lbxMovies.ItemIndex) then
  begin
    if lbxMovies.ItemIndex <> -1 then
    begin
      MovieTitle := lbxMovies.Items.Strings[lbxMovies.ItemIndex];

      if Trim(MovieTitle) <> '' then
      begin
        if MovieTitle[1] = '-' then MovieTitle[1] := '?'
        else if MovieTitle[1] = '?' then MovieTitle := Copy(MovieTitle, 2, Length(MovieTitle) - 1)
        else MovieTitle := '-' + MovieTitle;
        lbxMovies.Items.Strings[lbxMovies.ItemIndex] := MovieTitle;
      end;

      Filename := GetAppConfigDir(False) + 'MovieFileList.txt';
      lbxMovies.Items.SaveToFile(Filename)
    end;
  end;
end;

function TfrmMain.GetMovieInfo(Title: string; Year: integer; out Found: boolean): string;
var
  HTTPSender: THTTPSend;
  URL: string;
  Response: TStringStream;
begin
  Result := '';
  Found := False;
  HTTPSender := THTTPSend.Create;
  Response := TStringStream.Create('');
  try
    // URL encode the movie title
    Title := EncodeURLElement(Title);

    if (Year > 1900) and (Year < 2100) then
      Title := Title + '&y=' + IntToStr(Year);

    // Construct the URL with your API key
    URL := Format('http://www.omdbapi.com/?t=%s&apikey=%s', [Title, APIKEY]);

    // Perform the HTTP GET request
    if HTTPSender.HTTPMethod('GET', URL) then
    begin
      Response.CopyFrom(HTTPSender.Document, 0);
      Result := Response.DataString;

      Found := True;
    end
    else
      Result := 'Error retrieving data';
  finally
    HTTPSender.Free;
    Response.Free;
  end;
end;

function TfrmMain.ExtractMovieInfo(const JSONString: string): TMovieInfo;
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  RatingsArray: TJSONArray;
  MovieInfo: TMovieInfo;
begin
  // Initialize the result with empty strings
  MovieInfo.Title := '';
  MovieInfo.Year := 0;
  MovieInfo.Genre := '';
  MovieInfo.Actors := '';
  MovieInfo.Plot := '';
  MovieInfo.Ratings := '';
  MovieInfo.Poster := '';

  // Parse the JSON string
  JSONData := GetJSON(JSONString);
  try
    JSONObject := TJSONObject(JSONData);

    // Extract the required fields
    MovieInfo.Title := JSONObject.Get('Title', '');
    MovieInfo.Year := StrToIntDef(JSONObject.Get('Year', ''), -1);
    MovieInfo.Genre := JSONObject.Get('Genre', '');
    MovieInfo.Actors := JSONObject.Get('Actors', '');
    MovieInfo.Plot := JSONObject.Get('Plot', '');

    // Extract the Ratings from the first element in the "Ratings" array
    RatingsArray := TJSONArray(JSONObject.FindPath('Ratings'));
    if (RatingsArray <> nil) and (RatingsArray.Count > 0) then
    begin
      MovieInfo.Ratings := RatingsArray.AsJSON;
    end;

    MovieInfo.Poster := JSONObject.Get('Poster', '');
  finally
    JSONData.Free;
  end;

  Result := MovieInfo;
end;

function TfrmMain.GetCachedMovieInfo(const Filename, FilenameTitle: string; Year: integer): TMovieInfo;
var
  MovieInfo: TMovieInfo;
  JSONString: string;
  Found: boolean;
begin
  // Try to load movie info from the database
  if not FMovieDB.GetMovieInfo(Filename, MovieInfo) then
  begin
    // If not found in cache, fetch from the API
    JSONString := GetMovieInfo(FilenameTitle, Year, Found);
    if (Found) then
    begin
      MovieInfo := ExtractMovieInfo(JSONString);
      MovieInfo.FileName := FileName;

      // Save the new movie info to the database
      FMovieDB.SaveMovieInfo(MovieInfo);
    end;
  end;

  Result := MovieInfo;
end;

function TfrmMain.ExtractYearFromParentheses(var Input: string): Integer;
var
  StartPos, EndPos, LengthOfYear: Integer;
  YearString: string;
  YearValue: Integer;
begin
  Result := -1; // Default value indicating no valid year found

  StartPos := Pos('(', Input);
  EndPos := Pos(')', Input);

  if (StartPos > 0) and (EndPos > StartPos) then
  begin
    YearString := Trim(Copy(Input, StartPos + 1, EndPos - StartPos - 1));

    LengthOfYear := Length(YearString);
    YearValue := StrToIntDef(YearString, -1);

    // Check if the length is exactly 4 and all characters are digits
    if (LengthOfYear = 4) and (YearValue >= 1900) and (YearValue <= 2100) then
    begin
      Result := YearValue;
    end;

    Input := Trim(Copy(Input, 1, StartPos - 1) + Copy(Input, EndPos + 1, Length(Input)));
  end;
end;

function TfrmMain.DownloadImage(const URL: string; Stream: TMemoryStream): boolean;
var
  HTTPClient: THTTPSend;
begin
  Result := False;
  HTTPClient := THTTPSend.Create;
  try
    try
      HTTPClient.Sock.OnStatus := nil; // Disable status updates if not needed
      HTTPClient.HTTPMethod('GET', URL);

      if HTTPClient.ResultCode = 200 then
      begin
        Stream.LoadFromStream(HTTPClient.Document);
        Result := True;
      end;
    except
    end;
  finally
    HTTPClient.Free;
  end;
end;

procedure TfrmMain.LoadAndDisplayImage(const URL: string);
var
  ImageStream: TMemoryStream;
begin
  // Step 3: Download and display the image
  ImageStream := TMemoryStream.Create;
  imgPoster.Picture.Clear;
  try
    try
      ImageStream.Position := 0; // Ensure stream is at the start
      if DownloadImage(URL, ImageStream) then
      begin
        ImageStream.Position := 0;
        imgPoster.Picture.LoadFromStream(ImageStream);
      end;
    except
    end;
  finally
    ImageStream.Free;
  end;
end;

function TfrmMain.IsYear(const Str: string; out Year: Integer): Boolean;
begin
  // Assume a year is a four-digit number between 1900 and 2099
  Result := (Length(Str) = 4) and TryStrToInt(Str, Year) and (Year >= 1900) and (Year <= 2099);
end;

function TfrmMain.EndsWithYear(var Title: string; out Year: Integer): Boolean;
var
  LastSegment, NewTitle: string;
  SpacePos: Integer;
begin
  Result := False;
  Year := 0;

  // Trim leading and trailing whitespace
  Title := Trim(Title);

  // Find the position of the last space
  SpacePos := LastDelimiter(' ', Title);

  if SpacePos > 0 then
  begin
    // Extract the last segment after the last space
    LastSegment := Copy(Title, SpacePos + 1, Length(Title) - SpacePos);
    NewTitle :=  Copy(Title, 1, SpacePos - 1);

    // Check if the last segment is a valid year
    if IsYear(LastSegment, Year) then
    begin
      Title := NewTitle;
      Result := True;
    end;
  end;
end;

procedure TfrmMain.PlayMovie(const MovieFile: string);
var
  VLCPath: string;
  VLCProcess: TProcess;
begin
  // Check for the standard VLC installation
  if FileExists('/usr/bin/vlc') then
    VLCPath := '/usr/bin/vlc'
  // Check for VLC installed via Flatpak
  else if (ExecuteProcess('/usr/bin/flatpak', ['--version']) = 0) and
          (ExecuteProcess('/usr/bin/flatpak', ['info', 'org.videolan.VLC']) = 0) then
    VLCPath := '/usr/bin/flatpak'
  else
  begin
    raise Exception.Create('Error: VLC is not installed (neither as a standard package nor as a Flatpak).');
    Exit;
  end;

  // Check if the movie file exists
  if not FileExists(MovieFile) then
  begin
    raise Exception.Create('Error: Movie file "' +  MovieFile + '" not found.');
    Exit;
  end;

  // Start VLC process with the movie file
  VLCProcess := TProcess.Create(nil);
  try
    VLCProcess.Executable := VLCPath;
    if VLCPath = '/usr/bin/flatpak' then
      VLCProcess.Parameters.AddStrings(['run', 'org.videolan.VLC', MovieFile])
    else
      VLCProcess.Parameters.Add(MovieFile);

    VLCProcess.Options := [poWaitOnExit]; // Wait for VLC to finish
    VLCProcess.Execute;
  finally
    VLCProcess.Free;
  end;
end;

procedure TfrmMain.GetVideoFiles(const Folder: string; VideoFiles: TStrings);
var
  SearchRec: TSearchRec;
  FileMask, Extension: string;
begin
  // Check if the folder exists
  if not DirectoryExists(Folder) then
    raise Exception.Create('Directory does not exist: ' + Folder);

  // File search mask (all files in the folder)
  FileMask := Folder + DirectorySeparator + '*.*';

  // Start searching for files
  if FindFirst(FileMask, faAnyFile and not faDirectory, SearchRec) = 0 then
  try
    repeat
      Extension := LowerCase(ExtractFileExt(SearchRec.Name));
      // Check file extension and add to the list if it's a video file
      if (LowerCase(Extension) = '.mp4') or
         (LowerCase(Extension) = '.avi') or
         (LowerCase(Extension) = '.mkv') or
         (LowerCase(Extension) = '.mov') or
         (LowerCase(Extension) = '.flv') or
         (LowerCase(Extension) = '.wmv') then
      begin
        VideoFiles.Add(SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

function TfrmMain.ExtractRatingsAsList(const JSONString: string): String;
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  RatingsArray: TJSONArray;
  RatingsObj: TJSONObject;
  RatingsList: TStringList;
  i: Integer;
  Ratingstring: string;
begin
  Result := '';
  if JSONString = '' then Exit;

  try
    RatingsList := TStringList.Create;
    // Parse the input JSON string
    JSONData := GetJSON(JSONString);

    // Cast to JSONObject to access its fields
    if JSONData.JSONType = jtArray then
    begin
      // Extract the "Ratings" array
      RatingsArray := TJSONArray(JSONData);

      // Iterate through each element of the Ratings array
      for i := 0 to RatingsArray.Count - 1 do
      begin
        RatingsObj := RatingsArray.Objects[i]; // Access each object in the array

        // Format the Ratings as "Source: Value"
        Ratingstring := Format('%s: %s', [
          RatingsObj.Get('Source', 'Unknown Source'),
          RatingsObj.Get('Value', 'Unknown Value')
        ]);

        // Add the formatted string to the list
        RatingsList.Add(Ratingstring);
      end;
    end;

    Result := RatingsList.Text;
  finally
    JSONData.Free;
    RatingsList.Free;
  end;
end;

end.

