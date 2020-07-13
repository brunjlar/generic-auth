%include setup.fmt

\usepackage{epigraph}
\usepackage{tikz-cd}
\usepackage{MnSymbol}
\usepackage{mathtools}
\usepackage{caption}

\usetikzlibrary{decorations.pathmorphing}

\title{Authenticated Data Structures, Generically, in Haskell}
\date{2018-10-11}

%if style /= newcode
%format === = "\ensuremath{=}"
%else
%endif

\begin{document}

\maketitle

%if style == newcode
> {-# LANGUAGE DeriveGeneric      #-}
> {-# LANGUAGE DeriveAnyClass     #-}
> {-# LANGUAGE DeriveFunctor      #-}
> {-# LANGUAGE FlexibleContexts   #-}
> {-# LANGUAGE GADTs              #-}
> {-# LANGUAGE RankNTypes         #-}
> {-# LANGUAGE StandaloneDeriving #-}

> import           Control.Monad
> import           Control.Monad.Except    (MonadError(..), runExcept)
> import           Control.Monad.Reader    (MonadReader(..), runReaderT)
> import           Control.Monad.Writer    (MonadWriter (..), runWriter)
> import qualified Crypto.Hash             as C
> import           Data.Binary             (Binary (..), encode, decodeOrFail)
> import qualified Data.ByteArray          as A
> import qualified Data.ByteString         as SB
> import qualified Data.ByteString.Base16  as B16
> import           Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString)
> import qualified Data.ByteString.Builder as BB
> import qualified Data.ByteString.Char8   as B8
> import qualified Data.ByteString.Lazy    as LB
> import           GHC.Generics            (Generic)
> import           Prelude                 hiding (lookup)
%endif

\begin{frame}{Agenda}
    \begin{itemize}
        \item
            What are Authenticated data structures?
        \item
            Example: Merkle trees.
        \item
            A simple ad-hoc ADS.
        \item
            ADS's generically.
        \item
            Authenticated lists demo.
    \end{itemize}
\end{frame}

\begin{frame}{Authenticated data structures}
    \epigraph{An \emph{authenticated datastructure (ADS)}
        is a data structure whose operations can be carried out by an untrusted prover,
        the results of which a verifier can efficiently check as authentic.
    }{\textit{Andrew Miller et al.}}
\end{frame}

\begin{frame}{Rough idea}
How can this possibly work?\\[1cm]
\pause
The rough idea is to use (cryptographic) \alert{hashing}:
The verifier just needs hashe(s) of the datastructure(s),
the prover includes preimages to those hashes in its proofs.
\end{frame}

\begin{frame}{Cryptographic Hash functions}
    A (cryptographically secure) \emph{hash function}
    is a function that takes arbitrary bitstrings to
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

\begin{frame}{Merkle trees}
    In Bitcoin, Merkle trees are used to enable
    Simple Payment Verification (SPV) nodes.\vspace{-5mm}
    \begin{center}
        \includegraphics[width=0.8\textwidth,natwidth=800,natheight=509]{MerkleTree.svg}
        \captionof{figure}{Example Merkle tree \emph{(Wikipedia)}}
    \end{center}
\end{frame}

%if style /= newcode
%format T="\con{T}"
%format N="\con{N}"
%format Direction="\ty{Direction}"
%format L="\con{L}"
%format R="\con{R}"
%format Path="\ty{Path}"
%format Generic="\cl{Generic}"
%format Binary="\cl{Binary}"
%endif

\begin{frame}{A simple tree type}
Instead of Merkle trees, we will consider a very similar type
as our running example:\\
Let' define a type for simple binary trees with data in the leaves\ldots
> data Tree a = T a | N (Tree a) (Tree a)
>   deriving (Show, Generic, Binary)
\pause
\ldots and types representing paths in such trees:
> data Direction = L | R deriving Show
> type Path = [Direction]
\end{frame}

\begin{frame}[fragile]{Tree lookup}
Following a |Path|, we can |lookup| the value at the corresponding leaf
(ignoring partiality for simplicity's sake):
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

%if style /= newcode
%format hash'="\id{hash\rq}"
%endif

\begin{frame}{Tree lookup with proving \& verifying}
We want to to split |lookup| between a
\alert{prover} and a \alert{verifier}.

The prover holds the tree,
the verifier only knows the tree's
(modified) |hash'|:

> hash' :: Binary a => Tree a -> Hash
> hash'  (T a)    = hash a
> hash'  (N l r)  = hash (hash' l, hash' r)
\end{frame}

%if style /= newcode
%format prove="\id{prove}"
%format h1="\texttt{c6f318528e5de4532ec597d3be978c8e}"
%format h2="\texttt{e4151c03023facf27bb46dd21c5bf6fd}"
%format h3="\texttt{69503798ddf28ee3fa2358a5ab9def30}"
%format h4="\texttt{3fd723e4cfdb39c30f6352495b3c023f}"
%endif

\begin{frame}[fragile]{Proving tree lookup}
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

%if style /= newcode
%format verify="\id{verify}"
%endif

\begin{frame}[fragile]{Verifying tree lookup}
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

\begin{frame}{What we gain}
    \begin{itemize}
        \item<1->
            The \emph{verifier} only needs the |hash'| of the |Tree|, not the |Tree|
            itself (note that the proof size is logarithmic in the tree size!)
        \item<2>
            In order to cheat, the \emph{prover} would have to create a \alert{hash
            collision}.
    \end{itemize}
\end{frame}

\begin{frame}{Disadvantages}
    \begin{itemize}
        \item<1->
            We had to implement more or less the same algorithm twice,
            once for the prover, once for the verifier.
        \item<2->
            Functions |prove| and |verify| have to be carefully designed
            for this to work.
        \item<3->
            We had to come of with the custom hash function |hash'|.
        \item<4>
            If we want to use a data structure other than |Tree|
            or want to support more operations than just |lookup|,
            we have to think and work hard and do a new proof of correctness.
    \end{itemize}
\end{frame}

\begin{frame}{Authenticated Datastructures, Generically}
    In
    \emph{Authenticated Datastructures, Generically},
    Andrew Miller, Michael Hicks, Jonathan Katz, Elaine Shi,
    describe a generic way to construct authenticated datastructures
    in OCaml.
    \pause
    \begin{itemize}
        \item
            Extend the type system by adding a new type $\bullet a$ for each OCaml
            type $a$.\\
            \pause
        \item
            Formally add functions $\id{auth}:a\rightarrow\bullet a$ and
            $\id{unauth}:\bullet a\rightarrow a$ which are \emph{inverse}
            to each other.
            \pause
        \item
            Change the compiler, so that $\id{auth}$ and $\id{unauth}$
            are treated differently for prover and verifier.
            \pause
    \end{itemize}
    Instead of modifying GHC, we will instead use a \alert{free monad} to
    achieve a similar effect!
\end{frame}

\begin{frame}[fragile]{Authenticated Datastructures, Generically (cntd.)}
    \vspace{1cm}
    \begin{columns}
        \column{0.3\textwidth}
            \begin{minipage}[c][0.9\textheight][t]{\linewidth}
                \alert{Prover}
                \begin{itemize}
                    \item<2->
                        $\bullet a\sim a$.
                    \item<3->
                        $\id{auth}\ a$ does nothing.
                    \item<4->
                        $\id{unauth}\ x$ does nothing to get its result,
                        but writes $x$ to the proof-stream.
                \end{itemize}
            \end{minipage}
        \column{0.3\textwidth}
            \begin{minipage}[c][0.9\textheight][t]{\linewidth}
                \[
                    \begin{tikzcd}[row sep=5cm,column sep=tiny]%
                        a
                        \ar[d, bend right=30, "\id{auth}"'] \\
                        {\bullet a}
                        \ar[u, bend right=30, "\id{unauth}"'] \\
                    \end{tikzcd}
                \]
            \end{minipage}
        \column{0.3\textwidth}
            \begin{minipage}[c][0.9\textheight][t]{\linewidth}
                \alert{Verifier}
                \begin{itemize}
                    \item<2->
                        $\bullet a\sim\ty{Hash}$.
                    \item<3->
                        $\id{auth}\ a$ hashes $a$.
                    \item<4->
                        $\id{unauth}\ h$ reads $a$ from the
                        proof-stream and checks that |hash a == h|.
                \end{itemize}
            \end{minipage}
    \end{columns}
\end{frame}

%if style /= newcode
%format Auth="\ty{Auth}"
%format P="\con{P}"
%format V="\con{V}"
%format toHash="\id{toHash}"
%format put="\id{put}"
%format get="\id{get}"
%endif

\begin{frame}{|Auth|}
> data Auth a = P a | V Hash deriving Show

> toHash :: Binary a => Auth a -> Hash
> toHash (P a)  = hash a
> toHash (V h)  = h

> instance Binary a => Binary (Auth a) where
>   put = put . toHash
>   get = V <$> get
\pause
Crucially, when we serialize an |Auth a|,
we ``truncate'' it to its hash, so proofs
will be ``short''.
\end{frame}

%if style /= newcode
%format Free="\ty{Free}"
%format Pure="\con{Pure}"
%format MkFree="\con{Free}"
%endif

\begin{frame}{Reminder: |Free| monads}
> data Free f a =
>      Pure a
>   |  MkFree (f (Free f a))
>   deriving Functor

> instance Functor f => Applicative (Free f) where
>   pure = return
>   (<*>) = ap

> instance Functor f => Monad (Free f) where
>
>   return = Pure
>
>   Pure a    >>= cont  = cont a
>   MkFree f  >>= cont  = MkFree $ (>>= cont) <$> f
\end{frame}

%if style /= newcode
%format AuthF="\ty{AuthF}"
%format A="\con{A}"
%format U="\con{U}"
%format AuthM="\ty{AuthM}"
%format auth="\id{auth}"
%format unauth="\id{unauth}"
%endif

\begin{frame}{|AuthF| \& |AuthM|}
> data AuthF a where
>   A :: Binary b => b -> (Auth b -> a) -> AuthF a
>   U :: Binary b => Auth b -> (b -> a) -> AuthF a

> deriving instance Functor AuthF

> type AuthM a = Free AuthF a

> auth :: Binary a => a -> AuthM (Auth a)
> auth a = MkFree $ A a Pure

> unauth :: Binary a => Auth a -> AuthM a
> unauth x = MkFree $ U x Pure
\end{frame}

%if style /= newcode
%format ATree="\ty{Tree}"
%format AT="\con{T}"
%format AN="\con{N}"
%format alookup="\id{lookup}"
%endif

\begin{frame}{Authenticated trees}
Using |Auth|, we slightly modify our |Tree| type and |lookup|:
> data ATree a
>   = AT a | AN (Auth (ATree a)) (Auth (ATree a))
>   deriving (Show, Generic, Binary)

> alookup  :: Binary a
>          => Path -> Auth (ATree a) -> AuthM a
> alookup p x = do
>   t <- unauth x
>   case (p, t) of
>       ([]      , AT a)    -> return a
>       (d : ds  , AN l r)  -> alookup ds $
>           case d of L -> l; R -> r
\end{frame}

%if style /= newcode
%format runProver="\id{runProver}"
%format runProver'="\id{runProver\rq}"
%format runWriter="\id{runWriter}"
%format toLazyByteString="\id{toLazyByteString}"
%format MonadWriter="\cl{MonadWriter}"
%format Builder="\ty{Builder}"
%format LB.ByteString="\ty{ByteString}"
%format tell="\id{tell}"
%format lazyByteString="\id{lazyByteString}"
%format encode="\id{encode}"
%endif

\begin{frame}{Interpretation for the prover}
> runProver  :: MonadWriter Builder m
>            => AuthM a -> m a
> runProver  (Pure a)                 = return a
> runProver  (MkFree (A a cont))      =
>   runProver $ cont $ P a
> runProver  (MkFree (U (P a) cont))  = do
>   tell $ lazyByteString $ encode a
>   runProver $ cont a

> runProver' :: AuthM a -> (a, LB.ByteString)
> runProver' m =
>   let (a, b) = runWriter $ runProver m
>   in  (a, toLazyByteString b)
\end{frame}

%if style /= newcode
%format AuthError="\ty{AuthError}"
%format SerError="\con{SerError}"
%format HashMismatch="\con{HashMismatch}"
%endif

\begin{frame}{Interpretation for the verifier}
Verification can \alert{fail}, so let's define
an appropriate error type:
> data AuthError =
>      SerError String
>   |  HashMismatch
>   deriving Show
\end{frame}

%if style /= newcode
%format MonadError="\cl{MonadError}"
%format MonadReader="\cl{MonadReader}"
%format runVer="\id{runVer}"
%format runVer'="\id{runVer\rq}"
%format runReaderT="\id{runReaderT}"
%format runExcept="\id{runExcept}"
%format throwError="\id{throwError}"
%format decodeOrFail="\id{decodeOrFail}"
%format local="\id{local}"
%endif

\begin{frame}{Interpretation for the verifier (cntd.)}
\small
> runVer  ::  (  MonadReader LB.ByteString m
>             ,  MonadError AuthError m)
>         =>  AuthM a -> m a
> runVer (Pure a)              = return a
> runVer (MkFree (A a c))      = runVer $ c $ V $ hash a
> runVer (MkFree (U (V h) c))  = do
>   bs <- ask
>   case decodeOrFail bs of
>       Left (_, _, e)     -> throwError $ SerError e
>       Right (bs', _, a)
>           | hash a == h  -> local (const bs') $ runVer $ c a
>           | otherwise    -> throwError HashMismatch

> runVer' :: AuthM a -> LB.ByteString -> Either AuthError a
> runVer' m bs = runExcept $ runReaderT (runVer m) bs
\end{frame}

%if style /= newcode
%format at="\id{t}"
%format t'="\id{t\rq}"
%format ath = "\texttt{b5bd6ae28129b46d66d4f20924aa24ef}"
%endif

\begin{frame}{Revisiting our example}
\small
Let's recover our example tree in this setting!
> at, t' :: Auth (ATree Int)
> at = fst $ runProver' $ do
>   t1   <- auth $ AT 1
>   t2   <- auth $ AT 2
>   t12  <- auth $ AN t1 t2
>   t3   <- auth $ AT 3
>   auth $ AN t12 t3
> t' = V (toHash at)

< >>> at
< P (AN (P (AN (P (AT 1)) (P (AT 2)))) (P (AT 3)))
< >>> t'
< V ath
\end{frame}

\begin{frame}{Revisiting our example (cntd.)}
< >>> let proof =
<   snd $ runProver' [L,R] $ alookup at
< >>> runVer' (alookup [L,R] t') proof
< Right 2
< >>> runVer' (alookup [L,L] t') proof
< Left HashMismatch
\end{frame}

%if style /= newcode
%format AuthT="\ty{AuthT}"
%format MkAuthT="\con{AuthT}"
%format FreeT="\ty{FreeT}"
%endif

\begin{frame}{|AuthT|}
More generally, instead of using |Free| to define our monad |AuthM|,
we can use |FreeT| to define a \alert{monad transformer} |AuthT|:

< newtype AuthT m a = MkAuthT (FreeT AuthF m a)
\end{frame}

%if style /= newcode
%format AList="\ty{AList}"
%format Nil="\con{Nil}"
%format Cons="\con{Cons}"
%format push="\id{push}"
%format pop="\id{pop}"
%endif

\begin{frame}{Another example: authenticated lists (demo)}
As another simple example, we can define \alert{authenticated lists}\ldots

> data AList a = Nil | Cons a (Auth (AList a))
>   deriving (Show, Generic, Binary)

\ldots and use them to implement a simple stack API:
\small
\begin{overprint}
\onslide<1>
> push  :: Binary a
>       => a
>       -> Auth (AList a)
>       -> AuthM (Auth (AList a))
> push a x = auth $ Cons a x
\onslide<2>
> pop  :: Binary a
>      => Auth (AList a)
>      -> AuthM (Maybe a, Auth (AList a))
> pop l = do
>   xs <- unauth l
>   return $ case xs of
>       Nil        -> (Nothing, l)
>       Cons a l'  -> (Just a, l')
\end{overprint}
\end{frame}

%if style /= newcode
%format @ = \opsym{@}
%endif

\begin{frame}{Thank you for your attention!}
\includegraphics[width=2cm]{profile}
\begin{itemize}
    \item
        EMail: \texttt{lars.bruenjes@@iohk.io}
    \item
        Twitter: \texttt{@@LarsBrunjes}
    \item
        GitHub: \texttt{https://github.com/brunjlar/generic-auth}
\end{itemize}
\end{frame}

\end{document}
