\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{custom}[2025/03/22 Personal CV class]

\LoadClass{article}

% Make PDF builds reproducible
\ifx\pdfcreationdate\undefined\else
  \pdfinfoomitdate=1          % Omit CreationDate and ModDate
\fi
\ifx\pdfsuppressptexinfo\undefined\else
  \pdfsuppressptexinfo=-1     % Suppress all PTEX info
\fi
\pdftrailerid{}               % Use empty trailer ID instead of random
\pdfsuppresswarningpagegroup=1
\pdfcompresslevel=0           % Disable compression for reproducibility
\pdfobjcompresslevel=0        % Disable object stream compression

\RequirePackage{latexsym}
\RequirePackage[empty]{fullpage}
\RequirePackage[center, compact]{titlesec}
\RequirePackage{marvosym}
\RequirePackage[usenames,dvipsnames]{color}
\RequirePackage{verbatim}
\RequirePackage{enumitem}
\RequirePackage[hidelinks]{hyperref}

% Suppress hyperref's date metadata
\hypersetup{
  pdfcreationdate={},
  pdfmoddate={}
}

\RequirePackage{fancyhdr}
\RequirePackage[english]{babel}
\RequirePackage{tabularx}
\RequirePackage{xcolor}
\RequirePackage{multicol}
\RequirePackage[most]{tcolorbox}
\RequirePackage{changepage}
\RequirePackage{microtype}  % Better typography: subtle spacing and kerning improvements

\tcbset{
    frame code={},
    center title,
    left=0pt,
    right=0pt,
    top=0pt,
    bottom=0pt,
    colback=gray!10,
    colframe=white,
    width=\dimexpr\textwidth\relax,
    enlarge left by=0mm,
    arc=0pt,outer arc=0pt,
}

\definecolor{bluecv}{RGB}{42,72,121}

\pagestyle{fancy}
\fancyhf{} % clear all header and footer fields
\fancyfoot{}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}
\setlength{\headheight}{20pt}
\fancyhead[L]{\color{bluecv} \@author\space \textbar\space Page \thepage}
\renewcommand{\headrulewidth}{1pt}
\let\oldheadrule\headrule% Copy \headrule into \oldheadrule
\renewcommand{\headrule}{\color{bluecv}\oldheadrule}% Add colour to \headrule

\fancypagestyle{plain}{%
  \renewcommand{\headrulewidth}{0pt}%
  \fancyhf{}%
}
\thispagestyle{plain}

\newlength\FHoffset
\setlength\FHoffset{1cm}
\addtolength\headwidth{2\FHoffset}

\RequirePackage[
  top=-25pt,
  includehead,
  headheight=45pt,
  left=1cm,
  bottom=1.5cm,
  right=1cm,
  headsep=0.3cm
]{geometry}

\urlstyle{same}

% Suppress title rendering (#+TITLE is only used for PDF metadata)
\renewcommand{\maketitle}{}

\raggedbottom
\raggedright
\setlength{\tabcolsep}{0in}

% Line spacing control for page length
\setlength{\parskip}{0pt}
\renewcommand{\baselinestretch}{0.95}

% Section formatting
\titleformat{\section}{
\color{bluecv}\vspace{-2pt}\scshape\raggedright\large\centering\bfseries
}{}{0em}{}[\color{blue}\titlerule\vspace{-1pt}]

% Subsection formatting
\titleformat{\subsection}{
  \color{bluecv}\vspace{-4pt}\scshape\raggedright\large
}{\hspace{-.15in}}{0em}{}[\color{bluecv}\vspace{-8pt}]

% -------------------- CUSTOM COMMANDS --------------------
\newcommand{\resumeItem}[1]{
  \item\small{
    {#1 \vspace{-3pt}}
  }
}

\newcommand{\resumeSubheading}[4]{
  \vspace{-2pt}\item
    \begin{tabular*}{0.97\textwidth}[t]{l@{\extracolsep{\fill}}r}
      \textbf{#1} & #2 \\
      \textit{\small#3} & \textit{\small #4} \\
    \end{tabular*}\vspace{-7pt}
}

\newcommand{\resumeSubSubheading}[2]{
    \item
    \begin{tabular*}{0.97\textwidth}{l@{\extracolsep{\fill}}r}
      \textit{\small#1} & \textit{\small #2} \\
    \end{tabular*}\vspace{-7pt}
}

\newcommand{\resumeProjectHeading}[2]{
    \item
    \begin{tabular*}{0.97\textwidth}{l@{\extracolsep{\fill}}r}
      \small#1 & #2 \\
    \end{tabular*}\vspace{-7pt}
}

\newcommand{\resumeSubItem}[1]{\resumeItem{#1}\vspace{-4pt}}
\newcommand{\resumeSubHeadingListStart}{\begin{itemize}[leftmargin=0.15in, label={}]}
\newcommand{\resumeSubHeadingListEnd}{\end{itemize}}
\newcommand{\resumeItemListStart}{\begin{itemize}[topsep=0pt, partopsep=0pt, parsep=0pt]}
\newcommand{\resumeItemListEnd}{\end{itemize}\vspace{-7pt}}

\renewcommand\labelitemii{$\vcenter{\hbox{\tiny$\bullet$}}$}
\setlength{\footskip}{4.08003pt}

\newcommand{\heading}{
  \vspace*{-0.5cm} % Push upward more
  {\centering
    \color{bluecv} \textbf{\Huge \scshape \@author}\par
  }
  \vspace{-1.4em} % Reduce space after the heading (optional)
}

% Contact info storage
\newcommand{\@cvlocation}{}
\newcommand{\@cvphone}{}
\newcommand{\@cvphonedisplay}{}
\newcommand{\@cvusername}{}
\newcommand{\@cvwebsite}{}
\newcommand{\@cvemail}{}

% Setters for contact info (called from org via LATEX_HEADER)
\newcommand{\cvlocation}[1]{\renewcommand{\@cvlocation}{#1}}
\newcommand{\cvphone}[2]{\renewcommand{\@cvphone}{#1}\renewcommand{\@cvphonedisplay}{#2}}
\newcommand{\cvusername}[1]{\renewcommand{\@cvusername}{#1}}
\newcommand{\cvwebsite}[1]{\renewcommand{\@cvwebsite}{#1}}
\newcommand{\cvemail}[1]{\renewcommand{\@cvemail}{#1}}

% Render contact block using stored values (legacy)
\newcommand{\contact}{
  \begin{minipage}{4in}
  \small
  \@cvlocation \\
  \href{tel:\@cvphone}{\underline{\@cvphonedisplay}} $|$ \href{mailto:\@cvemail}{\underline{\@cvemail}}
  \end{minipage}
  \hfill
  \begin{minipage}{1.3in}
          \small
          \href{https://linkedin.com/in/\@cvusername}{\underline{LinkedIn Profile}}
          \href{\@cvwebsite}{\underline{Website}} $\vert$
          \href{https://github.com/\@cvusername}{\underline{Github}} \\
  \end{minipage}
}

% Render contact block with parameters
% #1: location, #2: phone, #3: phone display, #4: email, #5: username, #6: website
\newcommand{\contactinfo}[6]{
  \begin{minipage}{4in}
  \small
  #1 \\
  \href{tel:#2}{\underline{#3}} $|$ \href{mailto:#4}{\underline{#4}}
  \end{minipage}
  \hfill
  \begin{minipage}{1.3in}
          \small
          \href{https://linkedin.com/in/#5}{\underline{LinkedIn Profile}}
          \href{#6}{\underline{Website}} $\vert$
          \href{https://github.com/#5}{\underline{Github}} \\
  \end{minipage}
}

% Introduction storage
\newcommand{\@cvtagline}{}
\newcommand{\@cvsummary}{}

% Setters for introduction (called from org via LATEX_HEADER)
\newcommand{\cvtagline}[1]{\renewcommand{\@cvtagline}{#1}}
\newcommand{\cvsummary}[1]{\renewcommand{\@cvsummary}{#1}}

% Render introduction using stored values
\newcommand{\introduction}{
  \begin{center}
      \textbf{\@cvtagline}
  \end{center}%
  \begin{center}
      \small{\textbf{\textit{\@cvsummary}}}
  \end{center}
}

\newcommand{\position}[4]{
  \begin{itemize}[leftmargin=0pt, label={}]
    \item
    \begin{tabular*}{\textwidth}{l@{\extracolsep{\fill}}r}
      \small {\textcolor{bluecv}{\textbf{#1}}, #2, #3} & {#4} \\
    \end{tabular*}\vspace{-7pt}
  \end{itemize}
}

\newcommand{\priorheading}{
  \begin{itemize}[leftmargin=0.15in, label={}]
    \item
    \begin{tabular*}{0.97\textwidth}{l@{\extracolsep{\fill}}r}
      \small {\textbf{Prior Engagement:}} \\
    \end{tabular*}\vspace{-7pt}
  \end{itemize}
}

\newcommand{\priorposition}[4]{
  \begin{itemize}[leftmargin=0.15in, label={}]
    \item
    \begin{tabular*}{0.97\textwidth}{l@{\extracolsep{\fill}}r}
      \small {\textcolor{bluecv}{\textbf{#1}}, #2, #3} {(#4)} \\
    \end{tabular*}\vspace{-7pt}
  \end{itemize}
}

\newcommand{\lastupdate}{
  \begin{flushright}
    \color{gray!50}
    \textit{\small Last Updated on \today}
  \end{flushright}
}

\newenvironment{strengthstable}{%
  \begingroup
  \renewcommand{\arraystretch}{1.0}%
  \setlength{\tabcolsep}{8pt}%
  \begin{tabularx}{\textwidth}{XXX}}{%
  \end{tabularx}%
  \endgroup
  \vspace{-0.1cm}
}

\newenvironment{cvdescription}{
  \begin{tcolorbox}[width=\linewidth]}{
  \end{tcolorbox}
}

\AtEndDocument{%
  \lastupdate
}

% Ensure that generate pdf is machine readable/ATS parsable
\input{glyphtounicode}
\pdfgentounicode=1
