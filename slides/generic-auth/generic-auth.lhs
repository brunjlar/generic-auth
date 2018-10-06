%include setup.fmt

\usepackage{tikz-cd}
\usepackage{MnSymbol}
\usepackage{mathtools}

\usetikzlibrary{decorations.pathmorphing}

\title{Authenticated Data Structures, Generically, in Haskell}
\date{2018-10-11}

%if style /= newcode
%format === = "\ensuremath{=}"
%else
%endif

\begin{document}

\maketitle

\begin{frame}{Cryptographic Hash Functions}
    A (cryptographically secure) \emph{hash function}
    is a function $H$ that takes arbitrary bitstrings to
    bitstrings of a fixed length with the following
    additional properties:

    \begin{columns}[T]
        \column{0.4\textwidth}
            \begin{minipage}[c][0.5\textheight][c]{\linewidth}
                \begin{overprint}
                    \onslide<2>
                        \begin{itemize}
                            \item
                                \alert{collision resistant}
                        \end{itemize}
                    \onslide<3>
                        \begin{itemize}
                            \item
                                collision resistant
                            \item
                                \alert{hiding}
                        \end{itemize}
                \end{overprint}
            \end{minipage}
        \column{0.6\textwidth}
            \begin{minipage}[c][0.5\textheight][c]{\linewidth}
                \begin{overprint}
                    \onslide<2>
                        \begin{block}{collision resistant}
                            We are very unlikely to find two inputs which give the same output,
                            no matter how hard we try.
                        \end{block}
                    \onslide<3>
                        \begin{block}{hiding}
                            Given a hash, it is infeasible to find its associated input,
                            and the optimal way to do so is to try every possibility uniformly randomly.
                        \end{block}
                \end{overprint}
            \end{minipage}
    \end{columns}
\end{frame}

\begin{frame}{Hashing in Haskell}
%if style == newcode
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE DeriveAnyClass #-}

> import qualified Crypto.Hash            as C
> import           Data.Binary            (Binary (..), encode)
> import qualified Data.ByteArray         as A
> import qualified Data.ByteString        as SB
> import qualified Data.ByteString.Base16 as B16
> import qualified Data.ByteString.Char8  as B8
> import qualified Data.ByteString.Lazy   as LB
> import           GHC.Generics           (Generic)
> import           Prelude                hiding (lookup)

> newtype Hash = Hash {getHash :: C.Digest C.MD5}
>     deriving (Eq, Ord)

> instance Show Hash where
>     show = show . getHash

> instance Binary Hash where
>     get = do
>         md <- (C.digestFromByteString . SB.pack) <$> get
>         case md of
>             Nothing -> fail "expected hash"
>             Just d  -> return $ Hash d
>     put = put . A.unpack . getHash
%else
%format Hash="\ty{Hash}"
%format Binary="\cl{Binary}"
%format Generic="\cl{Generic}"
%format hash="\id{hash}"
%format hash42="\texttt{7e0535868cd45dff74884bfba0fa1594}"
In Haskell, we can use the excellent \alert{cryptonite} library for hashing.
For these slides, we'll use \alert{MD5}, but any hashing algorithm would do.
> data Hash = ...
%endif
> hash :: Binary a => a -> Hash
%if style == newcode
> hash = Hash . C.hash . LB.toStrict . encode
%endif
< >>> hash "Haskell"
< e74f20fc19d925fafccacc7ab837249e

< >>> hash (42 :: Int)
< hash42
\end{frame}

\begin{frame}{A Simple Tree Type}
%if style /= newcode
%format T="\con{T}"
%format N="\con{N}"
%format Direction="\ty{Direction}"
%format L="\con{L}"
%format R="\con{R}"
%format Path="\ty{Path}"
%endif
Let' define a type for simple binary trees with data in the leaves\ldots
> data Tree a = T a | N (Tree a) (Tree a)
>   deriving (Show, Generic, Binary)
\pause
\ldots and types representing paths in such trees:
> data Direction = L | R deriving Show
> type Path = [Direction]
\end{frame}

\begin{frame}[fragile]{Tree Lookup}
Following a |Path|, we can |lookup| the value at the corresponding leaf:
> lookup :: Path -> Tree a -> a
> lookup []        (T a)     = a
> lookup (d : ds)  (N l r)   =
>   lookup ds $ case d of L -> l; R -> r

\begin{columns}
\column{0.65\textwidth}
\small
< >>> let t = N (N (T 1) (T 2)) (T 3)
< >>> lookup [L,R] t
< 2
\column{0.25\textwidth}
\[
    \begin{tikzcd}[row sep=small,column sep=tiny]%
        & & \cdot \ar[ld] \ar[rd] \\
        & \cdot \ar[ld] \ar[rd] & & 3 \\
        1 & & 2 \\
    \end{tikzcd}
\]
\end{columns}
\end{frame}

\begin{frame}{Tree Lookup with Proving \& Verifying}
%if style /= newcode
%format hash'="\id{hash\rq}"
%endif
We want to to split |lookup| between a
\alert{prover} and a \alert{verifier}.

The prover holds the tree, 
the verifier only knows the tree's
(modified) |hash'|:

> hash' :: Binary a => Tree a -> Hash
> hash'  (T a)    = hash a
> hash'  (N l r)  = hash (hash' l, hash' r)
\end{frame}

\begin{frame}[fragile]{Proving Tree Lookup}
%if style /= newcode
%format prove="\id{prove}"
%format h1="\texttt{c6f318528e5de4532ec597d3be978c8e}"
%format h2="\texttt{e4151c03023facf27bb46dd21c5bf6fd}"
%format h3="\texttt{69503798ddf28ee3fa2358a5ab9def30}"
%format h4="\texttt{3fd723e4cfdb39c30f6352495b3c023f}"
%endif

> prove  :: Binary a
>        => Path -> Tree a -> (a, [(Hash, Hash)])
> prove []        (T a)     = (a, [])
> prove (d : ds)  (N l r)   =
>   let  (a, hs) = prove ds $ case d of L -> l; R -> r
>   in   (a, (hash' l, hash' r) : hs)

\begin{columns}
\column{0.65\textwidth}
< >>> prove [L,R] t
\fontsize{7}{8}
< (2,  [  (  h1
<         ,  h2)
<      ,  (  h3
<         ,  h4)
<      ])
\column{0.25\textwidth}
\[
    \begin{tikzcd}[row sep=small,column sep=tiny]%
        & & \cdot \ar[ld] \ar[rd] \\
        & \cdot \ar[ld] \ar[rd] & & 3 \\
        1 & & 2 \\
    \end{tikzcd}
\]
\end{columns}
\end{frame}

\begin{frame}[fragile]{Verifying Tree Lookup}
%if style /= newcode
%format verify="\id{verify}"
%endif

> verify  :: Binary a 
>         => Path -> Hash -> (a, [(Hash, Hash)]) 
>         -> Bool
> verify  []        h  (a, [])           = h == hash a
> verify  (d : xs)  h  (a, (l, r) : hs)  =
>     h  == hash (l, r)
>        && verify xs (case d of L -> l; R -> r) (a, hs)
> verify  _         _  _                 = False

\small
< >>> verify [L,R] (hash' t) $ prove [L,R] t
< True

< >>> verify [L,R] (hash' t) $ prove [L,L] t
< False
\end{frame}
\begin{frame}{IOHK is hiring Haskell Developers!}
\includegraphics{logo}
\end{frame}

\begin{frame}{Lars Br\"unjes}
%if style /= newcode
%format @ = \opsym{@}
%endif
\includegraphics[width=2cm]{profile}
\begin{itemize}
    \item
        EMail: \texttt{lars.bruenjes@@iohk.io}
    \item
        Twitter: \texttt{@@LarsBrunjes}
    \item
        GitHub: \texttt{brunjlar}
\end{itemize}
\end{frame}

\end{document}
