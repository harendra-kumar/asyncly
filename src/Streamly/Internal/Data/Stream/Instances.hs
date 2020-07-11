------------------------------------------------------------------------------
-- CPP macros for common instances
------------------------------------------------------------------------------

-- XXX use template haskell instead and include Monoid and IsStream instances
-- as well.

#define MONADPARALLEL , MonadAsync m

#define MONAD_COMMON_INSTANCES(STREAM,CONSTRAINT)                             \
instance Monad m => Functor (STREAM m) where {                                \
    {-# INLINE fmap #-};                                                      \
    fmap f (STREAM m) = D.fromStreamD $ D.mapM (return . f) $ D.toStreamD m;  \
    {-# INLINE (<$) #-};                                                      \
    (<$) =  fmap . const };                                                   \
                                                                              \
instance (MonadBase b m, Monad m CONSTRAINT) => MonadBase b (STREAM m) where {\
    liftBase = liftBaseDefault };                                             \
                                                                              \
instance (MonadIO m CONSTRAINT) => MonadIO (STREAM m) where {                 \
    liftIO = lift . liftIO };                                                 \
                                                                              \
instance (MonadThrow m CONSTRAINT) => MonadThrow (STREAM m) where {           \
    throwM = lift . throwM };                                                 \
                                                                              \
{- \
instance (MonadError e m CONSTRAINT) => MonadError e (STREAM m) where {       \
    throwError = lift . throwError;                                           \
    catchError m h =                                                          \
        fromStream $ withCatchError (toStream m) (\e -> toStream $ h e) };  \
-} \
                                                                              \
instance (MonadReader r m CONSTRAINT) => MonadReader r (STREAM m) where {     \
    ask = lift ask;                                                           \
    local f m = fromStream $ K.withLocal f (toStream m) };                    \
                                                                              \
instance (MonadState s m CONSTRAINT) => MonadState s (STREAM m) where {       \
    {-# INLINE get #-}; \
    get     = lift get;                                                       \
    {-# INLINE put #-}; \
    put x   = lift (put x);                                                   \
    {-# INLINE state #-}; \
    state k = lift (state k) }

------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

-- Serial streams can act like regular lists using the Identity monad

-- XXX Show instance is 10x slower compared to read, we can do much better.
-- The list show instance itself is really slow.

-- XXX The default definitions of "<" in the Ord instance etc. do not perform
-- well, because they do not get inlined. Need to add INLINE in Ord class in
-- base?

#if MIN_VERSION_deepseq(1,4,3)
#define NFDATA1_INSTANCE(STREAM)                                              \
instance NFData1 (STREAM Identity) where {                                    \
    {-# INLINE liftRnf #-};                                                   \
    liftRnf r = runIdentity . P.foldl' (\_ x -> r x) () }
#else
#define NFDATA1_INSTANCE(STREAM)
#endif

#define LIST_INSTANCES(STREAM)                                                \
instance IsList (STREAM Identity a) where {                                   \
    type (Item (STREAM Identity a)) = a;                                      \
    {-# INLINE fromList #-};                                                  \
    fromList = P.fromList;                                                    \
    {-# INLINE toList #-};                                                    \
    toList = runIdentity . P.toList };                                        \
                                                                              \
instance Eq a => Eq (STREAM Identity a) where {                               \
    {-# INLINE (==) #-};                                                      \
    (==) xs ys = runIdentity $ P.eqBy (==) xs ys };                           \
                                                                              \
instance Ord a => Ord (STREAM Identity a) where {                             \
    {-# INLINE compare #-};                                                   \
    compare xs ys = runIdentity $ P.cmpBy compare xs ys;                      \
    {-# INLINE (<) #-};                                                       \
    x <  y = case compare x y of { LT -> True;  _ -> False };                 \
    {-# INLINE (<=) #-};                                                      \
    x <= y = case compare x y of { GT -> False; _ -> True };                  \
    {-# INLINE (>) #-};                                                       \
    x >  y = case compare x y of { GT -> True;  _ -> False };                 \
    {-# INLINE (>=) #-};                                                      \
    x >= y = case compare x y of { LT -> False; _ -> True };                  \
    {-# INLINE max #-};                                                       \
    max x y = if x <= y then y else x;                                        \
    {-# INLINE min #-};                                                       \
    min x y = if x <= y then x else y; };                                     \
                                                                              \
instance Show a => Show (STREAM Identity a) where {                           \
    showsPrec p dl = showParen (p > 10) $                                     \
        showString "fromList " . shows (toList dl) };                         \
                                                                              \
instance Read a => Read (STREAM Identity a) where {                           \
    readPrec = parens $ prec 10 $ do {                                        \
        Ident "fromList" <- lexP;                                             \
        fromList <$> readPrec };                                              \
    readListPrec = readListPrecDefault };                                     \
                                                                              \
instance (a ~ Char) => IsString (STREAM Identity a) where {                   \
    {-# INLINE fromString #-};                                                \
    fromString = P.fromList };                                                \
                                                                              \
instance NFData a => NFData (STREAM Identity a) where {                       \
    {-# INLINE rnf #-};                                                       \
    rnf = runIdentity . P.foldl' (\_ x -> rnf x) () };                        \

-------------------------------------------------------------------------------
-- Foldable
-------------------------------------------------------------------------------

-- The default Foldable instance has several issues:
-- 1) several definitions do not have INLINE on them, so we provide
--    re-implementations with INLINE pragmas.
-- 2) the definitions of sum/product/maximum/minimum are inefficient as they
--    use right folds, they cannot run in constant memory. We provide
--    implementations using strict left folds here.

#define FOLDABLE_INSTANCE(STREAM)                                             \
instance (Foldable m, Monad m) => Foldable (STREAM m) where {                 \
                                                                              \
    {-# INLINE foldMap #-};                                                   \
    foldMap f = fold . P.foldr (mappend . f) mempty;                          \
                                                                              \
    {-# INLINE foldr #-};                                                     \
    foldr f z t = appEndo (foldMap (Endo #. f) t) z;                          \
                                                                              \
    {-# INLINE foldl' #-};                                                    \
    foldl' f z0 xs = foldr f' id xs z0                                        \
          where { f' x k z = k $! f z x};                                     \
                                                                              \
    {-# INLINE length #-};                                                    \
    length = foldl' (\n _ -> n + 1) 0;                                        \
                                                                              \
    {-# INLINE elem #-};                                                      \
    elem = any . (==);                                                        \
                                                                              \
    {-# INLINE maximum #-};                                                   \
    maximum =                                                                 \
          fromMaybe (errorWithoutStackTrace "maximum: empty stream")          \
        . toMaybe                                                             \
        . foldl' getMax Nothing' where {                                      \
            getMax Nothing' x = Just' x;                                      \
            getMax (Just' mx) x = Just' $! max mx x };                        \
                                                                              \
    {-# INLINE minimum #-};                                                   \
    minimum =                                                                 \
          fromMaybe (errorWithoutStackTrace "minimum: empty stream")          \
        . toMaybe                                                             \
        . foldl' getMin Nothing' where {                                      \
            getMin Nothing' x = Just' x;                                      \
            getMin (Just' mn) x = Just' $! min mn x };                        \
                                                                              \
    {-# INLINE sum #-};                                                       \
    sum = foldl' (+) 0;                                                       \
                                                                              \
    {-# INLINE product #-};                                                   \
    product = foldl' (*) 1 }

-------------------------------------------------------------------------------
-- Traversable
-------------------------------------------------------------------------------

#define TRAVERSABLE_INSTANCE(STREAM)                                          \
instance Traversable (STREAM Identity) where {                                \
    {-# INLINE traverse #-};                                                  \
    traverse f s = runIdentity $ P.foldr consA (pure mempty) s                \
        where { consA x ys = liftA2 K.cons (f x) ys }}
