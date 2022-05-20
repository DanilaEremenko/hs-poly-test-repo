module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование (из обычного списка первернутый)
listToRlist :: [a] -> ReverseList a
listToRlist lst = foldl (\acc el -> acc :< el) REmpty lst

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
    show lst = "[" ++ showRlist lst ++ "]"
        where
            showRlist lst = 
                case lst of
                    REmpty -> ""
                    (REmpty :< last) -> show last
                    (el1 :< el2) -> showRlist el1 ++ "," ++ show el2

instance (Eq a) => Eq (ReverseList a) 
    where
        (==) REmpty REmpty                     = True
        (==) REmpty _                          = False
        (==) _ REmpty                          = False
        (==) (head1 :< last1) (head2 :< last2) = head1 == head2 && last1 == last2

instance Semigroup (ReverseList a) 
    where
        (<>) REmpty REmpty = REmpty
        (<>) REmpty lst = lst
        (<>) lst REmpty = lst
        (<>) lst1 (head2 :< last2) = lst1 <> head2 :< last2

instance Monoid (ReverseList a) 
    where
            mempty = REmpty
            mappend = (<>)
            mconcat = foldl mappend mempty

instance Functor ReverseList
    where
        fmap f REmpty = notImplementedYet
instance Applicative ReverseList
    where
        (<*>) _ _ = notImplementedYet

instance Monad ReverseList where
