Attribute VB_Name = "ref_Book"
Option Explicit
Public wsf As WorksheetFunction
Public BibleBooks(1 To 67, 1 To 7) As Variant

Public Function Ref_Bk(Optn As Long, _
                       Optional Distinct = False, _
                       Optional Var = 0, _
                       Optional FilterVar As String) As Variant
                               
  Set wsf = Application.WorksheetFunction
  Dim i As Long, dm As Long, stdout As Variant, stdout1 As Variant, std As Variant
  Call BibleBks(BibleBooks)
  ReDim stdout(LBound(BibleBooks) + 1 To UBound(BibleBooks))
  
  If Var = 0 Or FilterVar = "0" Then
    For i = LBound(BibleBooks) + 1 To UBound(BibleBooks)
      stdout(i) = BibleBooks(i, Optn)
    Next i
  Else
    dm = 1
    For i = LBound(BibleBooks) + 1 To UBound(BibleBooks)
      If BibleBooks(i, Var) = FilterVar Then
        stdout(i) = BibleBooks(i, Optn)
        dm = dm + 1
      End If
    Next i
    'ReDim stdout(2 To dm)
    
  End If
  If Distinct = False Then
    Ref_Bk = wsf.Transpose(stdout)
  ElseIf Distinct = True Then
    Ref_Bk = wsf.Unique(wsf.Transpose(stdout))
  End If
End Function

Public Sub BibleBks(BibleBooks As Variant)
  BibleBooks(1, 1) = "OrderBk"
  BibleBooks(1, 2) = "Book"
  BibleBooks(1, 3) = "BookName"
  BibleBooks(1, 4) = "Testament_Ord"
  BibleBooks(1, 5) = "Testament"
  BibleBooks(1, 6) = "Section_Ord"
  BibleBooks(1, 7) = "Section"
  BibleBooks(2, 1) = "1"
  BibleBooks(2, 2) = "Gen"
  BibleBooks(2, 3) = "Genesis"
  BibleBooks(2, 4) = "1"
  BibleBooks(2, 5) = "Old"
  BibleBooks(2, 6) = "1"
  BibleBooks(2, 7) = "Law"
  BibleBooks(3, 1) = "2"
  BibleBooks(3, 2) = "Exo"
  BibleBooks(3, 3) = "Exodus"
  BibleBooks(3, 4) = "1"
  BibleBooks(3, 5) = "Old"
  BibleBooks(3, 6) = "1"
  BibleBooks(3, 7) = "Law"
  BibleBooks(4, 1) = "3"
  BibleBooks(4, 2) = "Lev"
  BibleBooks(4, 3) = "Leviticus"
  BibleBooks(4, 4) = "1"
  BibleBooks(4, 5) = "Old"
  BibleBooks(4, 6) = "1"
  BibleBooks(4, 7) = "Law"
  BibleBooks(5, 1) = "4"
  BibleBooks(5, 2) = "Num"
  BibleBooks(5, 3) = "Numbers"
  BibleBooks(5, 4) = "1"
  BibleBooks(5, 5) = "Old"
  BibleBooks(5, 6) = "1"
  BibleBooks(5, 7) = "Law"
  BibleBooks(6, 1) = "5"
  BibleBooks(6, 2) = "Deu"
  BibleBooks(6, 3) = "Deuteronomy"
  BibleBooks(6, 4) = "1"
  BibleBooks(6, 5) = "Old"
  BibleBooks(6, 6) = "1"
  BibleBooks(6, 7) = "Law"
  BibleBooks(7, 1) = "6"
  BibleBooks(7, 2) = "Jos"
  BibleBooks(7, 3) = "Joshua"
  BibleBooks(7, 4) = "1"
  BibleBooks(7, 5) = "Old"
  BibleBooks(7, 6) = "2"
  BibleBooks(7, 7) = "History"
  BibleBooks(8, 1) = "7"
  BibleBooks(8, 2) = "Jdg"
  BibleBooks(8, 3) = "Judges"
  BibleBooks(8, 4) = "1"
  BibleBooks(8, 5) = "Old"
  BibleBooks(8, 6) = "2"
  BibleBooks(8, 7) = "History"
  BibleBooks(9, 1) = "8"
  BibleBooks(9, 2) = "Rth"
  BibleBooks(9, 3) = "Ruth"
  BibleBooks(9, 4) = "1"
  BibleBooks(9, 5) = "Old"
  BibleBooks(9, 6) = "2"
  BibleBooks(9, 7) = "History"
  BibleBooks(10, 1) = "9"
  BibleBooks(10, 2) = "1Sa"
  BibleBooks(10, 3) = "1 Samuel"
  BibleBooks(10, 4) = "1"
  BibleBooks(10, 5) = "Old"
  BibleBooks(10, 6) = "2"
  BibleBooks(10, 7) = "History"
  BibleBooks(11, 1) = "10"
  BibleBooks(11, 2) = "2Sa"
  BibleBooks(11, 3) = "2 Samuel"
  BibleBooks(11, 4) = "1"
  BibleBooks(11, 5) = "Old"
  BibleBooks(11, 6) = "2"
  BibleBooks(11, 7) = "History"
  BibleBooks(12, 1) = "11"
  BibleBooks(12, 2) = "1Ki"
  BibleBooks(12, 3) = "1 Kings"
  BibleBooks(12, 4) = "1"
  BibleBooks(12, 5) = "Old"
  BibleBooks(12, 6) = "2"
  BibleBooks(12, 7) = "History"
  BibleBooks(13, 1) = "12"
  BibleBooks(13, 2) = "2Ki"
  BibleBooks(13, 3) = "2 Kings"
  BibleBooks(13, 4) = "1"
  BibleBooks(13, 5) = "Old"
  BibleBooks(13, 6) = "2"
  BibleBooks(13, 7) = "History"
  BibleBooks(14, 1) = "13"
  BibleBooks(14, 2) = "1Ch"
  BibleBooks(14, 3) = "1 Chronicles"
  BibleBooks(14, 4) = "1"
  BibleBooks(14, 5) = "Old"
  BibleBooks(14, 6) = "2"
  BibleBooks(14, 7) = "History"
  BibleBooks(15, 1) = "14"
  BibleBooks(15, 2) = "2Ch"
  BibleBooks(15, 3) = "2 Chronicles"
  BibleBooks(15, 4) = "1"
  BibleBooks(15, 5) = "Old"
  BibleBooks(15, 6) = "2"
  BibleBooks(15, 7) = "History"
  BibleBooks(16, 1) = "15"
  BibleBooks(16, 2) = "Ezr"
  BibleBooks(16, 3) = "Ezra"
  BibleBooks(16, 4) = "1"
  BibleBooks(16, 5) = "Old"
  BibleBooks(16, 6) = "2"
  BibleBooks(16, 7) = "History"
  BibleBooks(17, 1) = "16"
  BibleBooks(17, 2) = "Neh"
  BibleBooks(17, 3) = "Nehemiah"
  BibleBooks(17, 4) = "1"
  BibleBooks(17, 5) = "Old"
  BibleBooks(17, 6) = "2"
  BibleBooks(17, 7) = "History"
  BibleBooks(18, 1) = "17"
  BibleBooks(18, 2) = "Est"
  BibleBooks(18, 3) = "Esther"
  BibleBooks(18, 4) = "1"
  BibleBooks(18, 5) = "Old"
  BibleBooks(18, 6) = "2"
  BibleBooks(18, 7) = "History"
  BibleBooks(19, 1) = "18"
  BibleBooks(19, 2) = "Job"
  BibleBooks(19, 3) = "Job"
  BibleBooks(19, 4) = "1"
  BibleBooks(19, 5) = "Old"
  BibleBooks(19, 6) = "3"
  BibleBooks(19, 7) = "Wisdom"
  BibleBooks(20, 1) = "19"
  BibleBooks(20, 2) = "Psa"
  BibleBooks(20, 3) = "Psalms"
  BibleBooks(20, 4) = "1"
  BibleBooks(20, 5) = "Old"
  BibleBooks(20, 6) = "3"
  BibleBooks(20, 7) = "Wisdom"
  BibleBooks(21, 1) = "20"
  BibleBooks(21, 2) = "Pro"
  BibleBooks(21, 3) = "Proverbs"
  BibleBooks(21, 4) = "1"
  BibleBooks(21, 5) = "Old"
  BibleBooks(21, 6) = "3"
  BibleBooks(21, 7) = "Wisdom"
  BibleBooks(22, 1) = "21"
  BibleBooks(22, 2) = "Ecc"
  BibleBooks(22, 3) = "Ecclesiastes"
  BibleBooks(22, 4) = "1"
  BibleBooks(22, 5) = "Old"
  BibleBooks(22, 6) = "3"
  BibleBooks(22, 7) = "Wisdom"
  BibleBooks(23, 1) = "22"
  BibleBooks(23, 2) = "Sng"
  BibleBooks(23, 3) = "Song of Solomon"
  BibleBooks(23, 4) = "1"
  BibleBooks(23, 5) = "Old"
  BibleBooks(23, 6) = "3"
  BibleBooks(23, 7) = "Wisdom"
  BibleBooks(24, 1) = "23"
  BibleBooks(24, 2) = "Isa"
  BibleBooks(24, 3) = "Isaiah"
  BibleBooks(24, 4) = "1"
  BibleBooks(24, 5) = "Old"
  BibleBooks(24, 6) = "4"
  BibleBooks(24, 7) = "Maj Proph"
  BibleBooks(25, 1) = "24"
  BibleBooks(25, 2) = "Jer"
  BibleBooks(25, 3) = "Jeremiah"
  BibleBooks(25, 4) = "1"
  BibleBooks(25, 5) = "Old"
  BibleBooks(25, 6) = "4"
  BibleBooks(25, 7) = "Maj Proph"
  BibleBooks(26, 1) = "25"
  BibleBooks(26, 2) = "Lam"
  BibleBooks(26, 3) = "Lamentations"
  BibleBooks(26, 4) = "1"
  BibleBooks(26, 5) = "Old"
  BibleBooks(26, 6) = "4"
  BibleBooks(26, 7) = "Maj Proph"
  BibleBooks(27, 1) = "26"
  BibleBooks(27, 2) = "Eze"
  BibleBooks(27, 3) = "Ezekiel"
  BibleBooks(27, 4) = "1"
  BibleBooks(27, 5) = "Old"
  BibleBooks(27, 6) = "4"
  BibleBooks(27, 7) = "Maj Proph"
  BibleBooks(28, 1) = "27"
  BibleBooks(28, 2) = "Dan"
  BibleBooks(28, 3) = "Daniel"
  BibleBooks(28, 4) = "1"
  BibleBooks(28, 5) = "Old"
  BibleBooks(28, 6) = "4"
  BibleBooks(28, 7) = "Maj Proph"
  BibleBooks(29, 1) = "28"
  BibleBooks(29, 2) = "Hos"
  BibleBooks(29, 3) = "Hosea"
  BibleBooks(29, 4) = "1"
  BibleBooks(29, 5) = "Old"
  BibleBooks(29, 6) = "5"
  BibleBooks(29, 7) = "Min Proph"
  BibleBooks(30, 1) = "29"
  BibleBooks(30, 2) = "Joe"
  BibleBooks(30, 3) = "Joel"
  BibleBooks(30, 4) = "1"
  BibleBooks(30, 5) = "Old"
  BibleBooks(30, 6) = "5"
  BibleBooks(30, 7) = "Min Proph"
  BibleBooks(31, 1) = "30"
  BibleBooks(31, 2) = "Amo"
  BibleBooks(31, 3) = "Amos"
  BibleBooks(31, 4) = "1"
  BibleBooks(31, 5) = "Old"
  BibleBooks(31, 6) = "5"
  BibleBooks(31, 7) = "Min Proph"
  BibleBooks(32, 1) = "31"
  BibleBooks(32, 2) = "Oba"
  BibleBooks(32, 3) = "Obadiah"
  BibleBooks(32, 4) = "1"
  BibleBooks(32, 5) = "Old"
  BibleBooks(32, 6) = "5"
  BibleBooks(32, 7) = "Min Proph"
  BibleBooks(33, 1) = "32"
  BibleBooks(33, 2) = "Jon"
  BibleBooks(33, 3) = "Jonah"
  BibleBooks(33, 4) = "1"
  BibleBooks(33, 5) = "Old"
  BibleBooks(33, 6) = "5"
  BibleBooks(33, 7) = "Min Proph"
  BibleBooks(34, 1) = "33"
  BibleBooks(34, 2) = "Mic"
  BibleBooks(34, 3) = "Micah"
  BibleBooks(34, 4) = "1"
  BibleBooks(34, 5) = "Old"
  BibleBooks(34, 6) = "5"
  BibleBooks(34, 7) = "Min Proph"
  BibleBooks(35, 1) = "34"
  BibleBooks(35, 2) = "Nah"
  BibleBooks(35, 3) = "Nahum"
  BibleBooks(35, 4) = "1"
  BibleBooks(35, 5) = "Old"
  BibleBooks(35, 6) = "5"
  BibleBooks(35, 7) = "Min Proph"
  BibleBooks(36, 1) = "35"
  BibleBooks(36, 2) = "Hab"
  BibleBooks(36, 3) = "Habakkuk"
  BibleBooks(36, 4) = "1"
  BibleBooks(36, 5) = "Old"
  BibleBooks(36, 6) = "5"
  BibleBooks(36, 7) = "Min Proph"
  BibleBooks(37, 1) = "36"
  BibleBooks(37, 2) = "Zep"
  BibleBooks(37, 3) = "Zephaniah"
  BibleBooks(37, 4) = "1"
  BibleBooks(37, 5) = "Old"
  BibleBooks(37, 6) = "5"
  BibleBooks(37, 7) = "Min Proph"
  BibleBooks(38, 1) = "37"
  BibleBooks(38, 2) = "Hag"
  BibleBooks(38, 3) = "Haggai"
  BibleBooks(38, 4) = "1"
  BibleBooks(38, 5) = "Old"
  BibleBooks(38, 6) = "5"
  BibleBooks(38, 7) = "Min Proph"
  BibleBooks(39, 1) = "38"
  BibleBooks(39, 2) = "Zec"
  BibleBooks(39, 3) = "Zechariah"
  BibleBooks(39, 4) = "1"
  BibleBooks(39, 5) = "Old"
  BibleBooks(39, 6) = "5"
  BibleBooks(39, 7) = "Min Proph"
  BibleBooks(40, 1) = "39"
  BibleBooks(40, 2) = "Mal"
  BibleBooks(40, 3) = "Malachi"
  BibleBooks(40, 4) = "1"
  BibleBooks(40, 5) = "Old"
  BibleBooks(40, 6) = "5"
  BibleBooks(40, 7) = "Min Proph"
  BibleBooks(41, 1) = "40"
  BibleBooks(41, 2) = "Mat"
  BibleBooks(41, 3) = "Matthew"
  BibleBooks(41, 4) = "2"
  BibleBooks(41, 5) = "New"
  BibleBooks(41, 6) = "6"
  BibleBooks(41, 7) = "Gospels"
  BibleBooks(42, 1) = "41"
  BibleBooks(42, 2) = "Mar"
  BibleBooks(42, 3) = "Mark"
  BibleBooks(42, 4) = "2"
  BibleBooks(42, 5) = "New"
  BibleBooks(42, 6) = "6"
  BibleBooks(42, 7) = "Gospels"
  BibleBooks(43, 1) = "42"
  BibleBooks(43, 2) = "Luk"
  BibleBooks(43, 3) = "Luke"
  BibleBooks(43, 4) = "2"
  BibleBooks(43, 5) = "New"
  BibleBooks(43, 6) = "6"
  BibleBooks(43, 7) = "Gospels"
  BibleBooks(44, 1) = "43"
  BibleBooks(44, 2) = "Jhn"
  BibleBooks(44, 3) = "John"
  BibleBooks(44, 4) = "2"
  BibleBooks(44, 5) = "New"
  BibleBooks(44, 6) = "6"
  BibleBooks(44, 7) = "Gospels"
  BibleBooks(45, 1) = "44"
  BibleBooks(45, 2) = "Act"
  BibleBooks(45, 3) = "Acts"
  BibleBooks(45, 4) = "2"
  BibleBooks(45, 5) = "New"
  BibleBooks(45, 6) = "6"
  BibleBooks(45, 7) = "Gospels"
  BibleBooks(46, 1) = "45"
  BibleBooks(46, 2) = "Rom"
  BibleBooks(46, 3) = "Romans"
  BibleBooks(46, 4) = "2"
  BibleBooks(46, 5) = "New"
  BibleBooks(46, 6) = "7"
  BibleBooks(46, 7) = "Epistles Paul"
  BibleBooks(47, 1) = "46"
  BibleBooks(47, 2) = "1Co"
  BibleBooks(47, 3) = "1 Corinthians"
  BibleBooks(47, 4) = "2"
  BibleBooks(47, 5) = "New"
  BibleBooks(47, 6) = "7"
  BibleBooks(47, 7) = "Epistles Paul"
  BibleBooks(48, 1) = "47"
  BibleBooks(48, 2) = "2Co"
  BibleBooks(48, 3) = "2 Corinthians"
  BibleBooks(48, 4) = "2"
  BibleBooks(48, 5) = "New"
  BibleBooks(48, 6) = "7"
  BibleBooks(48, 7) = "Epistles Paul"
  BibleBooks(49, 1) = "48"
  BibleBooks(49, 2) = "Gal"
  BibleBooks(49, 3) = "Galatians"
  BibleBooks(49, 4) = "2"
  BibleBooks(49, 5) = "New"
  BibleBooks(49, 6) = "7"
  BibleBooks(49, 7) = "Epistles Paul"
  BibleBooks(50, 1) = "49"
  BibleBooks(50, 2) = "Eph"
  BibleBooks(50, 3) = "Ephesians"
  BibleBooks(50, 4) = "2"
  BibleBooks(50, 5) = "New"
  BibleBooks(50, 6) = "7"
  BibleBooks(50, 7) = "Epistles Paul"
  BibleBooks(51, 1) = "50"
  BibleBooks(51, 2) = "Phl"
  BibleBooks(51, 3) = "Philippians"
  BibleBooks(51, 4) = "2"
  BibleBooks(51, 5) = "New"
  BibleBooks(51, 6) = "7"
  BibleBooks(51, 7) = "Epistles Paul"
  BibleBooks(52, 1) = "51"
  BibleBooks(52, 2) = "Col"
  BibleBooks(52, 3) = "Colossians"
  BibleBooks(52, 4) = "2"
  BibleBooks(52, 5) = "New"
  BibleBooks(52, 6) = "7"
  BibleBooks(52, 7) = "Epistles Paul"
  BibleBooks(53, 1) = "52"
  BibleBooks(53, 2) = "1Th"
  BibleBooks(53, 3) = "1 Thessalonians"
  BibleBooks(53, 4) = "2"
  BibleBooks(53, 5) = "New"
  BibleBooks(53, 6) = "7"
  BibleBooks(53, 7) = "Epistles Paul"
  BibleBooks(54, 1) = "53"
  BibleBooks(54, 2) = "2Th"
  BibleBooks(54, 3) = "2 Thessalonians"
  BibleBooks(54, 4) = "2"
  BibleBooks(54, 5) = "New"
  BibleBooks(54, 6) = "7"
  BibleBooks(54, 7) = "Epistles Paul"
  BibleBooks(55, 1) = "54"
  BibleBooks(55, 2) = "1Ti"
  BibleBooks(55, 3) = "1 Timothy"
  BibleBooks(55, 4) = "2"
  BibleBooks(55, 5) = "New"
  BibleBooks(55, 6) = "7"
  BibleBooks(55, 7) = "Epistles Paul"
  BibleBooks(56, 1) = "55"
  BibleBooks(56, 2) = "2Ti"
  BibleBooks(56, 3) = "2 Timothy"
  BibleBooks(56, 4) = "2"
  BibleBooks(56, 5) = "New"
  BibleBooks(56, 6) = "7"
  BibleBooks(56, 7) = "Epistles Paul"
  BibleBooks(57, 1) = "56"
  BibleBooks(57, 2) = "Tit"
  BibleBooks(57, 3) = "Titus"
  BibleBooks(57, 4) = "2"
  BibleBooks(57, 5) = "New"
  BibleBooks(57, 6) = "7"
  BibleBooks(57, 7) = "Epistles Paul"
  BibleBooks(58, 1) = "57"
  BibleBooks(58, 2) = "Phm"
  BibleBooks(58, 3) = "Philemon"
  BibleBooks(58, 4) = "2"
  BibleBooks(58, 5) = "New"
  BibleBooks(58, 6) = "7"
  BibleBooks(58, 7) = "Epistles Paul"
  BibleBooks(59, 1) = "58"
  BibleBooks(59, 2) = "Heb"
  BibleBooks(59, 3) = "Hebrews"
  BibleBooks(59, 4) = "2"
  BibleBooks(59, 5) = "New"
  BibleBooks(59, 6) = "8"
  BibleBooks(59, 7) = "Epistles Gen"
  BibleBooks(60, 1) = "59"
  BibleBooks(60, 2) = "Jas"
  BibleBooks(60, 3) = "James"
  BibleBooks(60, 4) = "2"
  BibleBooks(60, 5) = "New"
  BibleBooks(60, 6) = "8"
  BibleBooks(60, 7) = "Epistles Gen"
  BibleBooks(61, 1) = "60"
  BibleBooks(61, 2) = "1Pe"
  BibleBooks(61, 3) = "1 Peter"
  BibleBooks(61, 4) = "2"
  BibleBooks(61, 5) = "New"
  BibleBooks(61, 6) = "8"
  BibleBooks(61, 7) = "Epistles Gen"
  BibleBooks(62, 1) = "61"
  BibleBooks(62, 2) = "2Pe"
  BibleBooks(62, 3) = "2 Peter"
  BibleBooks(62, 4) = "2"
  BibleBooks(62, 5) = "New"
  BibleBooks(62, 6) = "8"
  BibleBooks(62, 7) = "Epistles Gen"
  BibleBooks(63, 1) = "62"
  BibleBooks(63, 2) = "1Jo"
  BibleBooks(63, 3) = "1 John"
  BibleBooks(63, 4) = "2"
  BibleBooks(63, 5) = "New"
  BibleBooks(63, 6) = "8"
  BibleBooks(63, 7) = "Epistles Gen"
  BibleBooks(64, 1) = "63"
  BibleBooks(64, 2) = "2Jo"
  BibleBooks(64, 3) = "2 John"
  BibleBooks(64, 4) = "2"
  BibleBooks(64, 5) = "New"
  BibleBooks(64, 6) = "8"
  BibleBooks(64, 7) = "Epistles Gen"
  BibleBooks(65, 1) = "64"
  BibleBooks(65, 2) = "3Jo"
  BibleBooks(65, 3) = "3 John"
  BibleBooks(65, 4) = "2"
  BibleBooks(65, 5) = "New"
  BibleBooks(65, 6) = "8"
  BibleBooks(65, 7) = "Epistles Gen"
  BibleBooks(66, 1) = "65"
  BibleBooks(66, 2) = "Jde"
  BibleBooks(66, 3) = "Jude"
  BibleBooks(66, 4) = "2"
  BibleBooks(66, 5) = "New"
  BibleBooks(66, 6) = "8"
  BibleBooks(66, 7) = "Epistles Gen"
  BibleBooks(67, 1) = "66"
  BibleBooks(67, 2) = "Rev"
  BibleBooks(67, 3) = "Revelation"
  BibleBooks(67, 4) = "2"
  BibleBooks(67, 5) = "New"
  BibleBooks(67, 6) = "9"
  BibleBooks(67, 7) = "Epistle Apoc"
End Sub
