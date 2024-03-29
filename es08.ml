( * - - - - - - - - - - - - -   G r u p p o 8   - - - - - - - - - - - - - * )  
  
  
 ( * = = = = = = = = = = = =   E s 1 a : s u b e x p r   = = = = = = = = = = = = = * )  
 t y p e   e x p r   =    
         I n t   o f   i n t  
     |   V a r   o f   s t r i n g  
     |   S u m   o f   e x p r   *   e x p r  
     |   D i f f   o f   e x p r   *   e x p r  
     |   M u l t   o f   e x p r   *   e x p r  
     |   D i v   o f   e x p r   *   e x p r  
  
 ( *   s u b e x p r :   e x p r   - >   e x p r   - >   b o o l   * )  
 l e t   r e c   s u b e x p r   e 1   e 2   =  
     e 1 = e 2   o r  
     m a t c h   e 1   w i t h  
         S u m ( x , y )   |   D i f f ( x , y )   |   M u l t ( x , y )   |   D i v ( x , y )   - >    
             s u b e x p r   x   e 2   | |   s u b e x p r   y   e 2  
     |   _   - >   f a l s e  
  
 ( * = = = = = = = = = = = =   E s 1 b : s u b s t - i n - e x p r   = = = = = = = = = = = = = * )  
 ( *   s u b s t _ i n _ e x p r :   e x p r   - >   s t r i n g   - >   e x p r   - >   e x p r   * )  
 l e t   r e c   s u b s t _ i n _ e x p r   e   x   e '   =  
     m a t c h   e   w i t h  
         V a r   y   - >   i f   x = y   t h e n   e '   e l s e   e  
     |   I n t   _   - >   e  
     |   S u m ( a , b )   - >   S u m ( s u b s t _ i n _ e x p r   a   x   e ' , s u b s t _ i n _ e x p r   b   x   e ' )  
     |   D i f f ( a , b )   - >   D i f f ( s u b s t _ i n _ e x p r   a   x   e ' , s u b s t _ i n _ e x p r   b   x   e ' )  
     |   M u l t ( a , b )   - >   M u l t ( s u b s t _ i n _ e x p r   a   x   e ' , s u b s t _ i n _ e x p r   b   x   e ' )  
     |   D i v ( a , b )   - >   D i v ( s u b s t _ i n _ e x p r   a   x   e ' , s u b s t _ i n _ e x p r   b   x   e ' )  
  
 ( * = = = = = = = = = = = =   E s 2 a : r e f l e c t   = = = = = = = = = = = = = * )  
 t y p e   ' a   t r e e   =   E m p t y   |   T r   o f   ' a   *   ' a   t r e e   *   ' a   t r e e  
  
 ( *   r e f l e c t :       ' a   t r e e   - >   ' a   t r e e   * )  
 l e t   r e c   r e f l e c t   =   f u n c t i o n  
         E m p t y   - >   E m p t y  
     |   T r ( x , t 1 , t 2 )   - >   T r ( x , r e f l e c t   t 2 , r e f l e c t   t 1 ) ; ;  
  
 ( * = = = = = = = = = = = =   E s 2 b : f u l l t r e e   = = = = = = = = = = = = = * )  
  
 ( *   f u l l t r e e :   i n t   - >   i n t   t r e e   * )  
     ( *   a u x   :   i n t   - >   i n t   - >   i n t   t r e e   * )  
     ( *   a u x   k   n   =   a l b e r o   c o m p l e t o   c o n   r a d i c e   k ,   a l t e z z a   n ,   e   n o d i  
                               e t i c h e t t a t i   c o m e   r i c h i e s t o   d a l l ' e s e r c i z i o   * )  
 l e t   f u l l t r e e   n   =    
     l e t   r e c   a u x   k   n   =  
         i f   n = 0   t h e n   E m p t y  
         e l s e   T r ( k , a u x   ( 2 * k )   ( n - 1 ) , a u x   ( 2 * k + 1 )   ( n - 1 ) )  
     i n   a u x   1   n    
  
 ( * = = = = = = = = = = = =   E s 2 c : b a l a n c e d   = = = = = = = = = = = = = * )  
 ( *   p e r   v e r i f i c a r e   s e   u n   a l b e r o   e '   b i l a n c i a t o ,    
       o c c o r r e   s a p e r   c a l c o l a r e   l ' a l t e z z a   d i   u n   a l b e r o .  
       N e l   c a s o   d e g l i   a l b e r i   b i n a r i   r a p p r e s e n t a t i   c o n   l ' a l b e r o  
       v u o t o ,   p o s s i a m o   d e f i n i r l a   c o s i '   p e r   e v i t a r e   d i   a v e r e    
       a l t e z z e   n e g a t i v e :   * )  
 ( *   h e i g h t :   ' a   t r e e   - >   i n t    
         h e i g h t   t   =   a l t e z z a   d i   t   * )  
 l e t   r e c   h e i g h t   =   f u n c t i o n  
         E m p t y   - >   0  
     |   T r ( _ , t 1 , t 2 )   - >   1   +   m a x   ( h e i g h t   t 1 )   ( h e i g h t   t 2 ) ; ;  
  
 ( *   b a l a n c e d :   ' a   t r e e   - >   b o o l   * )  
 l e t   r e c   b a l a n c e d   =   f u n c t i o n  
         E m p t y   - >   t r u e  
     |   T r ( _ , t 1 , t 2 )   - >  
               b a l a n c e d   t 1   & &   b a l a n c e d   t 2  
               & &   a b s ( h e i g h t   t 1   -   h e i g h t   t 2 )   < =   1 ; ;  
  
 ( *   l a   d e f i n i z i o n e   p r e c e d e n t e   i m p l e m e n t a   u n   a l g o r i t m o   c o s t o s o :    
       p e r   o g n i   c h i a m a t a   r i c o r s i v a ,   i   d u e   s o t t o a l b e r i   v e n g o n o   v i s i t a t i  
       d u e   v o l t e ,   u n a   p e r   v e r i f i c a r e   s e   s o n o   b i l a n a c i a t i ,   e   u n ' a l t r a  
       p e r   d e t e r m i n a r n e   l ' a l t e z z a .   E '   p o s s i b i l e   f a r e   l e   d u e   c o s e  
       c o n t e m p o r a n e a m e n t e ,   m e d i a n t e   u n a   f u n z i o n e   c h e   r i p o r t a   l ' a l t e z z a  
       d i   u n   a l b e r o   s e   e '   b i l a n c i a t o ,   e   s o l l e v a   u n ' e c c e z i o n e   s e   n o n    
       e '   b i l a n c i a t o   ( l a   f u n z i o n e   l o c a l e   a u x   n e l   p r o g r a m m a   q u i   s o t t o )   * )  
 ( *   a u x   :   ' a   t r e e   - >   i n t  
       a u x   t   =   a l t e z z a   d i   t ,   s e   t   e '   b i l a n c i a t o ,   a l t r i m e n t i   s o l l e v a  
                       N o t B a l a n c e d   * )  
 e x c e p t i o n   N o t B a l a n c e d ; ;  
  
 l e t   b a l a n c e d   t   =  
     l e t   r e c   a u x   =   f u n c t i o n  
             E m p t y   - >   0  
         |   T r ( _ , t 1 , t 2 )   - >  
                           l e t   k 1   =   a u x   t 1  
                           a n d   k 2   =   a u x   t 2  
                           i n   i f   a b s ( k 1   -   k 2 )   < =   1  
                                 t h e n   1   +   m a x   k 1   k 2  
                                 e l s e   r a i s e   N o t B a l a n c e d  
     i n   t r y   l e t   _   =   a u x   t   i n   t r u e  
           w i t h   N o t B a l a n c e d   - >   f a l s e ; ;  
  
 ( * = = = = = = = = = = = =   E s 2 d : p r e o r d e r - p o s t o r d e r - i n o r d e r   = = = = = = = = = = = = = * )  
 ( *   t i p o   d e l l e   t r e   f u n z i o n i   p e r   l a   v i s i t a   d i   a l b e r i   b i n a r i :  
                   ' a   t r e e   - >   ' a   l i s t   * )  
 l e t   r e c   p r e o r d e r   =   f u n c t i o n  
         E m p t y   - >   [ ]  
     |   T r ( x , t 1 , t 2 )   - >     x : : ( p r e o r d e r   t 1   @   p r e o r d e r   t 2 )  
  
 l e t   r e c   i n o r d e r   =   f u n c t i o n  
         E m p t y   - >   [ ]  
     |   T r ( x , t 1 , t 2 )   - >   ( i n o r d e r   t 1 )   @   ( x : : ( i n o r d e r   t 2 ) )  
  
 l e t   r e c   p o s t o r d e r   =   f u n c t i o n  
         E m p t y   - >   [ ]  
     |   T r ( x , t 1 , t 2 )   - >   ( p o s t o r d e r   t 1 )   @   ( ( p o s t o r d e r   t 2 )   @   [ x ] )  
  
 ( * = = = = = = = = = = = =   E s 2 e : b a l p r e o r d e r - b a l i n o r d e r   = = = = = = = = = = = = = * )  
 ( *   p e r   l a   d e f i n i z i o n e   d i   q u e s t e   f u n z i o n i   u t i l i z z i a m o   l e   f u n z i o n i    
       t a k e   e   d r o p ,   d e f i n i t e ,   r i s p e t t i v a m e n t e ,   a   l e z i o n e   e   n e l l ' e s e r c i z i o  
       1 d   d e l   G r u p p o   4   * )  
 ( *   t a k e   :   i n t   - >   ' a   l i s t   - >   ' a   l i s t   * )  
 l e t   r e c   t a k e   n   =   f u n c t i o n  
         [ ]   - >   [ ]  
     |   x : : x s   - >   i f   n < = 0   t h e n   [ ]  
                           e l s e   x : : t a k e   ( n - 1 )   x s  
  
 ( *   d r o p   :   i n t   - >   ' a   l i s t   - >   ' a   l i s t   * )  
 l e t   r e c   d r o p   n   =   f u n c t i o n  
         [ ]   - >   [ ]  
     |   x : : x s   - >   i f   n < = 0   t h e n   x : : x s  
                           e l s e   d r o p   ( n - 1 )   x s ; ;  
  
 ( *   b a l p r e o r d e r   :   ' a   l i s t   - >   ' a   t r e e   * )  
 l e t   r e c   b a l p r e o r d e r   =   f u n c t i o n  
         [ ]   - >   E m p t y  
     |   x : : x s   - >  
         l e t   k   =   ( L i s t . l e n g t h   x s ) /   2  
         i n   T r ( x ,   b a l p r e o r d e r   ( t a k e   k   x s ) ,  
                           b a l p r e o r d e r   ( d r o p   k   x s ) )  
  
 ( *   b a l i n o r d e r   :   ' a   l i s t   - >   ' a   t r e e   * )  
 l e t   r e c   b a l i n o r d e r   =   f u n c t i o n  
         [ ]   - >   E m p t y  
     |   x s   - >   l e t   k   =   ( L i s t . l e n g t h   x s ) /   2  
                     i n   l e t   y : : y s   =   d r o p   k   x s  
                     i n   T r ( y ,   b a l i n o r d e r   ( t a k e   k   x s ) ,  
                                       b a l i n o r d e r   y s ) ; ;  
  
 ( * = = = = = = = = = = = =   E s 3 : f o g l i e - i n - l i s t a   = = = = = = = = = = = = = * )  
 ( *   f o g l i e _ i n _ l i s t a :   ' a   l i s t   - >   ' a   t r e e   - >   b o o l   * )  
 l e t   r e c   f o g l i e _ i n _ l i s t a   l i s t a   =   f u n c t i o n  
         E m p t y   - >   t r u e  
     |   T r ( x , E m p t y , E m p t y )   - >   L i s t . m e m   x   l i s t a  
     |   T r ( x , l e f t , r i g h t )   - >  
             f o g l i e _ i n _ l i s t a   l i s t a   l e f t   & &  
             f o g l i e _ i n _ l i s t a   l i s t a   r i g h t  
  
 ( * = = = = = = = = = = = =   E s 4 : n u m - f o g l i e   = = = = = = = = = = = = = * )  
 ( *   n u m _ f o g l i e :   ' a   t r e e   - >   i n t   * )  
 l e t   r e c   n u m _ f o g l i e   =   f u n c t i o n  
         E m p t y   - >   0  
     |   T r ( x , E m p t y , E m p t y )   - >   1  
     |   T r ( x , l e f t , r i g h t )   - >   ( n u m _ f o g l i e   l e f t )   +   ( n u m _ f o g l i e   r i g h t )  
  
 ( * = = = = = = = = = = = =   E s 5 : s e g u i - b o o l   = = = = = = = = = = = = = * )  
 ( *   s e g u i _ b o o l   :   b o o l   l i s t   - >   ' a   t r e e   - >   ' a   * )  
 l e t   r e c   s e g u i _ b o o l   l i s t a   t r e e   =    
     m a t c h   ( t r e e , l i s t a )   w i t h    
       ( E m p t y , _ )   - >   f a i l w i t h   " s e g u i   b o o l "  
     |   ( T r ( x , _ , _ ) , [ ] )   - >   x  
     |   ( T r ( _ , t 1 , t 2 ) , y : : r e s t )   - >    
             i f   y   t h e n   s e g u i _ b o o l   r e s t   t 1  
             e l s e   s e g u i _ b o o l   r e s t   t 2  
  
 ( * = = = = = = = = = = = =   E s 6 : f o g l i a - c o s t o   = = = = = = = = = = = = = * )  
 ( *   f o g l i a _ c o s t o   :   i n t   t r e e   - >   i n t   *   i n t   * )  
 l e t   r e c   f o g l i a _ c o s t o   =   f u n c t i o n  
         E m p t y   - >   f a i l w i t h   " f o g l i a _ c o s t o "  
     |   T r ( x , E m p t y , E m p t y )   - >   ( x , x )  
     |   T r ( x , l e f t , E m p t y )   - >  
             l e t   ( y , c )   =   f o g l i a _ c o s t o   l e f t  
             i n   ( y , c + x )  
     |   T r ( x , E m p t y , r i g h t )   - >  
             l e t   ( y , c )   =   f o g l i a _ c o s t o   r i g h t  
             i n   ( y , c + x )  
     |   T r ( x , l e f t , r i g h t )   - >  
             l e t   ( y l e f t , c l e f t )   =   f o g l i a _ c o s t o   l e f t   i n  
             l e t   ( y r i g h t , c r i g h t )   =   f o g l i a _ c o s t o   r i g h t   i n  
             i f   c l e f t   > =   c r i g h t   t h e n   ( y l e f t , c l e f t + x )  
             e l s e   ( y r i g h t , c r i g h t + x )  
  
 ( * = = = = = = = = = = = =   E s 7 : f o g l i e - c o s t i   = = = = = = = = = = = = = * )  
 ( *     f o g l i e _ c o s t i   :   i n t   t r e e   - >   ( i n t   *   i n t )   l i s t   * )  
 l e t   r e c   f o g l i e _ c o s t i   =   f u n c t i o n  
         E m p t y   - >   [ ]  
     |   T r ( x , E m p t y , E m p t y )   - >   [ ( x , x ) ]  
     |   T r ( x , l e f t , r i g h t )   - >  
               L i s t . m a p   ( f u n c t i o n   ( y , c )   - >   ( y , x + c ) )  
 	   ( ( f o g l i e _ c o s t i   l e f t )   @   ( f o g l i e _ c o s t i   r i g h t ) )  
  
 ( * = = = = = = = = = = = =   E s 8 : p a t t e r n - m a t c h i n g   = = = = = = = = = = = = = * )  
 t y p e   e x p r   =  
         J o l l y  
     |   I n t   o f   i n t  
     |   V a r   o f   s t r i n g  
     |   S u m   o f   e x p r   *   e x p r  
     |   D i f f   o f   e x p r   *   e x p r  
     |   M u l t   o f   e x p r   *   e x p r  
     |   D i v   o f   e x p r   *   e x p r  
  
 ( *   p a t t e r n _ m a t c h i n g   :   e x p r   - >   e x p r   - >   b o o l   * )  
 l e t   r e c   p a t t e r n _ m a t c h i n g   e   p a t t e r n   =    
     m a t c h   ( e , p a t t e r n )   w i t h    
     |   ( _ ,   J o l l y )   - >   t r u e  
     |   ( S u m ( e 1 , e 2 ) , S u m ( m 1 , m 2 ) )  
     |   ( D i f f ( e 1 , e 2 ) , D i f f ( m 1 , m 2 ) )  
     |   ( M u l t ( e 1 , e 2 ) , M u l t ( m 1 , m 2 ) )    
     |   ( D i v ( e 1 , e 2 ) , D i v ( m 1 , m 2 ) )     - >  
             p a t t e r n _ m a t c h i n g   e 1   m 1   & &   p a t t e r n _ m a t c h i n g   e 2   m 2  
     |   ( e , m )   - >   e   =   m  
  
 ( * = = = = = = = = = = = =   E s 9 : m a x - c o m m o n - s u b t r e e   = = = = = = = = = = = = = * )  
 ( *   m a x _ c o m m o n _ s u b t r e e   :   s t r i n g   t r e e   - >   s t r i n g   t r e e   - >   s t r i n g   t r e e   * )  
 l e t   r e c   m a x _ c o m m o n _ s u b t r e e   t r e e 1   t r e e 2   =  
     m a t c h   ( t r e e 1 , t r e e 2 )   w i t h  
         ( E m p t y , E m p t y )   - >   E m p t y  
     |   ( T r ( _ , _ , _ ) , E m p t y )  
     |   ( E m p t y , T r ( _ , _ , _ ) )   - >   T r ( " @ " , E m p t y , E m p t y )  
     |   ( T r ( a , t 1 , t 2 ) , T r ( b , r 1 , r 2 ) )   - >    
             i f   a = b   t h e n   T r ( a , m a x _ c o m m o n _ s u b t r e e   t 1   r 1 , m a x _ c o m m o n _ s u b t r e e   t 2   r 2 )  
             e l s e   T r ( " @ " , E m p t y , E m p t y )  
  
 ( * = = = = = = = = = = = =   E s 1 0 a : s t e s s a - s t r u t t u r a   = = = = = = = = = = = = = * )  
 ( *   s t e s s a _ s t r u t t u r a   :   ' a   t r e e   - >   ' b   t r e e   - >   b o o l   * )  
 l e t   r e c   s t e s s a _ s t r u t t u r a   t 1   t 2   =    
     m a t c h   ( t 1 , t 2 )   w i t h    
     |   ( E m p t y , E m p t y )   - >   t r u e  
     |   ( T r ( _ , t 1 , t 2 ) , T r ( _ , u 1 , u 2 ) )   - >    
             s t e s s a _ s t r u t t u r a   t 1   u 1   & &   s t e s s a _ s t r u t t u r a   t 2   u 2  
     |   _   - >   f a l s e      
  
 ( * = = = = = = = = = = = =   E s 1 0 b : e s i s t e - m a p p i n g   = = = = = = = = = = = = = * )  
 ( *   i s _ f u n c t i o n :   ( ' a   *   ' b )   l i s t   - >   b o o l    
       i s _ f u n c t i o n   l s t   =   t r u e   s e   l a   l i s t a   a s s o c i a t i v a   l s t   r a p p r e s e n t a    
       u n a   f u n z i o n e   * )  
 l e t   r e c   i s _ f u n c t i o n   =   f u n c t i o n  
         [ ]   - >   t r u e  
     |   ( x , y ) : : r e s t   - >    
             L i s t . f o r _ a l l   ( f u n c t i o n   ( z , w )   - >   z < > x   o r   w = y )   r e s t  
 	 & &   i s _ f u n c t i o n   r e s t  
  
 ( *   i n   r e a l t a '   l ' u s o   d i   L i s t . f o r _ a l l   n e l l a   d e f i n i z i o n e   d a t a   s o p r a   d i  
       i s _ f u n c t i o n   r e n d e   i l   c o d i c e   c o m p a t t o ,   m a   e '   p i u '   p e s a n t e   d e l  
       n e c e s s a r i o ,   d a l   p u n t o   d i   v i s t a   c o m p u t a z i o n a l e .   N o n   e '   n e c e s s a r i o ,  
       p e r   o g n i   ( x , y )   n e l l a   l i s t a ,   s c a n d i r e   t u t t o   i l   r e s t o   d e l l a   l i s t a ,   m a  
       b a s t a   c o n t r o l l a r e   p e r   l ' e v e n t u a l e   v a l o r e   d e l l a   f o r m a   ( x , z )  
       s u c c e s s i v o   s e   y = z :   * )  
  
 l e t   r e c   i s _ f u n c t i o n   =   f u n c t i o n  
         [ ]   - >   t r u e  
     |   ( x , y ) : : r e s t   - >    
                     t r y   l e t   z   =   L i s t . a s s o c   x   r e s t    
                             i n   z = y   & &   i s _ f u n c t i o n   r e s t  
                     w i t h   N o t _ f o u n d   - >   i s _ f u n c t i o n   r e s t  
  
 ( *   m a p s t o   :   ' a   t r e e   - >   ' b   t r e e   - >   ( ' a   *   ' b )   l i s t   * )  
 ( *   m p a s t o   t 1   t 2   =   l i s t a   d i   c o p p i e   c h e   r a p p r e s e n t a   u n   m a p p i n g  
       d i   t r a s f o r m a z i o n e   d a   t 1   a   t 2 .   F a l l i s c e   s e   t 1   e   t 2   n o n   h a n n o    
       l a   s t e s s a   s t r u t t u r a   * )  
 l e t   r e c   m a p s t o   t 1   t 2   =   m a t c h   ( t 1 , t 2 )   w i t h  
         ( E m p t y , E m p t y )   - >   [ ]  
     |   ( T r ( x , l e f t 1 , r i g h t 1 ) , T r ( y , l e f t 2 , r i g h t 2 ) )   - >  
             ( x , y ) : : ( m a p s t o   l e f t 1   l e f t 2 ) @ ( m a p s t o   r i g h t 1   r i g h t 2 )  
     |   _   - >   f a i l w i t h   " E r r o r e "  
  
 ( *   e s i t e _ m a p p i n g   :   ' a   t r e e   - >   ' b   t r e e   - >   b o o l   * )  
 l e t   e s i t e _ m a p p i n g   t 1   t 2   =  
     t r y   i s _ f u n c t i o n   ( m a p s t o   t 1   t 2 )  
     w i t h   _   - >   f a l s e  
  
 ( * = = = = = = = = = = = =   E s 1 1 : p a t h - s e n z a - p   = = = = = = = = = = = = = * )  
 ( *   p a t h   :   ( ' a   - >   b o o l )   - >   ' a   t r e e   - >   ' a   l i s t   * )  
 l e t   r e c   p a t h   p   =       f u n c t i o n        
         E m p t y   - >   f a i l w i t h   " E r r o r e "  
     |   T r   ( x , E m p t y , E m p t y )   - >    
             i f   p   x   t h e n     f a i l w i t h   " E r r o r e "  
             e l s e   [ x ]  
     |   T r ( x , t 1 , t 2 )   - >  
             i f   p   x   t h e n     f a i l w i t h   " E r r o r e "  
             e l s e   x : : ( t r y   p a t h   p   t 1  
                               w i t h   F a i l u r e   " E r r o r e "   - >   p a t h   p   t 2 )  
  
 ( * = = = = = = = = = = = =   E s 1 2 : a p p l i c a - s u b s t   = = = = = = = = = = = = = * )  
 t y p e   ' a   s o s t i t u z i o n e   =   ( ' a   *   ' a   t r e e )   l i s t  
 ( *   a p p l i c a   :   s o s t i t u z i o n e   - >   ' a   t r e e   - >   ' a   t r e e   * )  
 l e t   r e c   a p p l i c a   l i s t   =   f u n c t i o n  
 	 |   E m p t y   - >   E m p t y  
 	 |   T r ( x , E m p t y , E m p t y )   a s   t e m p   - >    
                                 ( t r y   L i s t . a s s o c   x   l i s t  
 	 	   w i t h   _   - >   t e m p )  
 	 |   T r ( x , t 1 , t 2 )   - >   T r ( x , a p p l i c a   l i s t   t 1 , a p p l i c a   l i s t   t 2 )  
  
 ( * = = = = = = = = = = = =   E s 1 3 : p a t h - c o p r e n t e - l i s t a   = = = = = = = = = = = = = * )  
  
 ( *     f u n z i o n e   a u s i l i a r i a  
         t o g l i   :   ' a   - >   ' a   l i s t   - >   ' a   l i s t  
         t o g l i   x   l s t   =   l i s t a   c h e   s i   o t t i e n e   e l i m i n a n d o   u n ' o c c o r r e n z a   d i   x   d a   l s t ,  
                                         s e   c ' e ' ,   a l t r i m e n t i   l s t   s t e s s a   * )  
 l e t   r e c   t o g l i   x   =   f u n c t i o n  
         [ ]   - >   [ ]  
     |   y : : r e s t   - >  
             i f   x = y   t h e n   r e s t    
             e l s e   y : : t o g l i   x   r e s t  
  
 ( *   p a t h _ c o p r e n t e   :   ' a   t r e e   - >   ' a   l i s t   - >   ' a   l i s t   * )  
 l e t   r e c   p a t h _ c o p r e n t e   t r e e   l i s t   =  
     m a t c h   t r e e   w i t h  
         E m p t y   - >   f a i l w i t h   " E r r o r e "  
     |   T r ( x , E m p t y , E m p t y )   - >  
                     i f   l i s t = [ ]   | |   l i s t = [ x ]   t h e n   [ x ]  
                     e l s e   f a i l w i t h   " E r r o r e "  
     |   T r ( x , t 1 , t 2 )   - >  
                     l e t   n e w l i s t   =   t o g l i   x   l i s t   i n  
                     x : : ( t r y   p a t h _ c o p r e n t e   t 1   n e w l i s t  
                             w i t h   _   - >   p a t h _ c o p r e n t e   t 2   n e w l i s t )  
  
 ( * = = = = = = = = = = = =   E s 1 4 : p a t h - c o l o r i - a l t e r n i   = = = = = = = = = = = = = * )  
  
 t y p e   c o l   =   R o s s o   |   G i a l l o   |   V e r d e   |   B l u  
 t y p e   ' a   c o l _ a s s o c   =   ( c o l   *   ' a   l i s t )   l i s t  
  
 ( *   ( a )   * )  
 ( *   c o l o r e   :   ' a   - >   ( ' b   *   ' a   l i s t )   l i s t   - >   ' b   * )  
 ( *   q u i n d i   a n c h e  
       c o l o r e :   ' a   - >   ' a   c o l _ a s s o c   - >   c o l   * )  
 l e t   c o l o r e   n   c o l o r i   =  
     l e t   r e c   a u x   =   f u n c t i o n  
             [ ]   - >   f a i l w i t h   " c o l o r e "  
         |   ( c , l s t ) : : r e s t   - >  
 	 i f   L i s t . m e m   n   l s t   t h e n   c   e l s e   a u x   r e s t  
     i n   a u x   c o l o r i  
  
 ( *   o p p u r e   * )  
 l e t   c o l o r e   n   c o l o r i   =  
     t r y   f s t   ( L i s t . f i n d   ( f u n   ( _ , l s t )   - >   L i s t . m e m   n   l s t )   c o l o r i )  
     w i t h   _   - >   f a i l w i t h   " c o l o r e "  
  
 ( *   ( b )   * )  
 ( *   p a t h _ t o :   ' a   - >   ' a   c o l _ a s s o c   - >   ' a   t r e e   - >   ' a   l i s t   * )  
 l e t   p a t h _ t o   n   c o l o r i   t   =  
     l e t   r e c   a u x   p a d r e   =   f u n c t i o n  
             E m p t y   - >   r a i s e   N o t _ f o u n d  
         |   T r ( x , E m p t y , E m p t y )   - >  
 	 i f   x = n   & &   c o l o r e   x   c o l o r i   < >   p a d r e  
 	 t h e n   [ x ]   e l s e   r a i s e   N o t _ f o u n d  
         |   T r ( x , t 1 , t 2 )   - >  
 	 l e t   n e w c o l   =   c o l o r e   x   c o l o r i   i n  
 	 i f   n e w c o l   =   p a d r e    
 	 t h e n   r a i s e   N o t _ f o u n d  
 	 e l s e   x : : t r y   a u x   n e w c o l   t 1  
 	 w i t h   N o t _ f o u n d   - >   a u x   n e w c o l   t 2  
     i n   m a t c h   t   w i t h  
         E m p t y   - >   r a i s e   N o t _ f o u n d  
     |   T r ( x , E m p t y , E m p t y )   - >  
             i f   x = n   t h e n   [ x ]   e l s e   r a i s e   N o t _ f o u n d  
     |   T r ( x , t 1 , t 2 )   - >    
 	 l e t   c o l   =   c o l o r e   x   c o l o r i   i n  
 	 x : : t r y   a u x   c o l   t 1  
 	 w i t h   N o t _ f o u n d   - >   a u x   c o l   t 2  
  
 ( * = = = = = = = = = = = =   E s 1 5 a : a b r - c h e c k   = = = = = = = = = = = = = * )  
  
 ( *   t r e e _ f o r _ a l l   p   t   =   t r u e   s e   t u t t i   i   n o d i   d i   t   s o d d i s f a n o   i l   p r e d i c a t o   p   * )  
 ( *   t r e e _ f o r _ a l l :   ( ' a   - >   b o o l )   - >   ' a   t r e e   - >   b o o l   * )  
 l e t   r e c   t r e e _ f o r _ a l l   p   =   f u n c t i o n  
         E m p t y   - >   t r u e  
     |   T r ( x , t 1 , t 2 )   - >   p   x   & &   t r e e _ f o r _ a l l   p   t 1   & &   t r e e _ f o r _ a l l   p   t 2  
  
 ( *   a b r _ c h e c k :     ( ' a   *   ' b )   t r e e   - >   b o o l   * )  
 l e t   r e c   a b r _ c h e c k   =   f u n c t i o n  
       E m p t y   - >   t r u e  
   |   T r ( ( k , _ ) , t 1 , t 2 )   - >  
             t r e e _ f o r _ a l l   ( f u n c t i o n   ( k 1 , _ )   - >   k 1   <   k )   t 1   & &  
             t r e e _ f o r _ a l l   ( f u n c t i o n   ( k 1 , _ )   - >   k 1   >   k )   t 2   & &  
             a b r _ c h e c k   t 1   & &   a b r _ c h e c k   t 2  
  
 ( *   l ' a l g o r t i m o   d i c h i a r a t i v o   d a t o   s o p r a   e '   c h i a r a m e n t e   m o l t o  
       i n e f f i c i e n t e .     E '   p o s s i b i l e   c o n t r o l l a r e   l a   p r o p r i e t a '   A B R   v i s i t a n d o  
       o g n i   n o d o   d e l l ' a l b e r o   u n a   s o l a   v o l t a .   I l   t r u c c o   c o n s i s t e   n e l l ' u s a r e  
       u n a   f u n z i o n e   a u s i l i a r i a   ( l a   f u n z i o n e   a b r _ u t i l   n e l   c o d i c e   s o t t o  
       r i p o r t a t o ) ,   c o n   d u e   a r g o m e n t i   s u p p l e m e n t a r i ,   p e r   r a p p r e s e n t a r e ,  
       r i s p e t t i v a m e n t e ,   i l   m i n i m o   e   i l   m a s s i m o   v a l o r e   a m m e s s o   p e r   l e  
       c h i a v i   d e i   n o d i .   T a l i   v a l o r i   d i v e n t a n o   v i a   v i a   p i u '   s t r i n g e n t i :  
       q u a n d o   s i   v i s i t a   i l   s o t t o a l b e r o   s i n i s t r o   d i   u n   a l b e r o   c o n   c h i a v e   k  
       a l l a   r a d i c e ,   i   v a l o r i   d o v r a n n o   e s s e r e   s e m p r e   i n f e r i o r i   a   k ,   q u i n d i ,  
       s e   k   d o v e v a   e s s e r e   i n f e r i o r e   a   u n   v a l o r e   m a s s i m o   d a t o ,   n e l  
       s o t t o a l b e r o   s i n i s t r o   l e   c h i a v i   d o v r a n n o   e s s e r e   i n f e r i o r i   a   k :   i l  
       m a s s i m o   v i e n e   s o s t i t u i t o   d a   k .   V i s i t a n d o   i l   s o t t o a l b e r o   d e s t r o ,   s i  
       m o d i f i c h e r a '   s i m m e t r i c a m e n t e   i l   v a l o r e   m i n i m o   a m m e s s o .     P e r   n o n  
       a v e r e   p r o b l e m i   c o n   i   v a l o r i   d i   p a r t e n z a   ( n o n   d e f i n i t i )   u s i a m o   i l  
       t i p o   ' a   o p t i o n   p e r   r a p p r e s e n t a r e   i   v a l o r i   m i n i m i   e   m a s s i m i   a m m e s s i ,  
       e   d e f i n i a m o   o p e r a z i o n i   d i   c o n f r o n t o   t r a   ' a   o p t i o n ,   i n   m o d o   c h e   i l  
       c o n f r o n t o   c o n   N o n e   s i a   s e m p r e   t r u e .   S i   n o t i   c h e   l ' u s o   d i   m i n _ i n t   e  
       m a x _ i n t   v i n c o l e r e b b e   l e   c h i a v i   a d   a v e r e   t i p o   i n t   * )  
  
 ( *   c o n f r o n t o   t r a   u n   ' a   e   u n   ' a   o p t i o n :   s e m p r e   v e r o   s e   i l   s e c o n d o  
       a r g o m e n t o   e '   N o n e ,   a l t r i m e n t i ,   s e   i l   s e c o n d o   a r g o m e n t o   e '   V a l   y ,  
       c o n f r o n t a   ( c o n   <   o   > ,   r i s p e t t i v a m e n t e )   i l   p r i m o   a r g o m e n t o   c o n   x   * )  
 ( *   (   < <   )   :   ' a   - >   ' a   o p t i o n   - >   b o o l    
       (   > >   )   :   ' a   - >   ' a   o p t i o n   - >   b o o l   * )  
 l e t   ( < < )   a   =   f u n c t i o n  
         N o n e   - >   t r u e  
     |   S o m e   b   - >   a < b  
 l e t   ( > > )   a   =   f u n c t i o n  
         N o n e   - >   t r u e  
     |   S o m e   b   - >   a > b  
    
 ( *   a b r _ u t i l :   ' a   o p t i o n   - >   ' a   o p t i o n   - >   ( ' a   *   ' b )   t r e e   - >   b o o l   * )  
 ( *   a b r _ u t i l   m i n v   m a x v   t   =   t r u e   s e   t   e '   u n   a b r   e   t u t t i   i   s u o i   n o d i   s o n o  
       > >   m i n v   e   < <   m a x v   * )  
 l e t   a b r _ c h e c k   t   =  
     l e t   r e c   a b r _ u t i l   m i n v   m a x v   =   f u n c t i o n  
             E m p t y   - >   t r u e  
         |   T r ( ( x , _ ) , l e f t , r i g h t )   - >  
 	 x   > >   m i n v   & &   x   < <   m a x v   & &  
 	 a b r _ u t i l   m i n v   ( S o m e   x )   l e f t   & &  
 	 a b r _ u t i l   ( S o m e   x )   m a x v   r i g h t  
     i n   a b r _ u t i l   N o n e   N o n e   t  
  
 ( *   u n     a l g o r i t m o   a l t e r n a t i v o   c h e   v i s i t a   c i a s c u n   n o d o   u n a   v o l t a   s o l a  
       s i   b a s a   s u   u n a   v i s i t a   i n   p o s t o r d i n e   d e l l ' a l b e r o ,   n e l l a   q u a l e   s i   c a l c o l a n o  
       i   v a l o r i   d e l l ' e t i c h e t t a   m i n i m a   e   d i   q u e l l a   m a s s i m a   d i   c i a s c u n   s o t t o a l b e r o ,  
       e   s i   c o n f r o n t a n o   c o n   l a   r a d i c e .  
       L ' i m p l e m e n t a z i o n e   s o t t o   p r o p o s t a   f a   u s o   d i   u n a   f u n z i o n e   a u s i l i a r i a  
                     a u x   :   ' a   n t r e e   - >   ' a   *   ' a  
       c h e ,   a p p l i c a t a   a   u n   a l b e r o   t ,   s o l l e v a   l ' e c c e z i o n e   F a i l u r e   " E m p t y "   s e    
       t   =   E m p t y ,  
       s o l l e v a   F a i l u r e   " F a l s e "   s e   t   n o n   e '   u n   a b r ,   a l t r i m e n t i   r i p o r t a   l a   c o p p i a  
       ( t _ m i n , t _ m a x )   d o v e   t _ m i n   e '   l ' e t i c h e t t a   m i n i m a   i n   t ,   e   t _ m a x   e '   l ' e t i c h e t t a  
       m a s s i m a   i n   t .  
       N e l   c a s o   g e n e r a l e ,   p e r   c o n t r o l l a r e   s e   u n   a l b e r o   T r ( x , l e f t , r i g h t )   e '   u n   a b r ,  
       l a   f u n z i o n e   c o n t r o l l a   s e   x   e '   m a g g i o r e   d e l l ' e t i c h e t t a   m i n i m a   d i   l e f t   e    
       m a g g i o r e   d e l l ' e t i c h e t t a   m a s s i m a   d i   r i g h t .  
       L a   f u n z i o n e   p r i n c i p a l e   r i c h i a m a   a u x   s u l l ' a l b e r o   s e m p l i c e m e n t e   p e r   c o n t r o l l a r e  
       s e   v i e n e   s o l l e v a t a   l ' e c c e z i o n e   F a i l u r e   " F a l s e " ,   n e l   q u a l   c a s o   r i p o r t a   f a l s e .  
       I n   t u t t i   g l i   a l t r i   c a s i   r i p o r t a   t r u e .     * )      
 l e t   a b r _ c h e c k   t r e e   =    
     l e t   r e c   a u x   =   f u n c t i o n  
             E m p t y   - >   f a i l w i t h   " E m p t y "  
         |   T r ( x , l e f t , r i g h t )   - >    
 	 m a t c h   ( l e f t , r i g h t )   w i t h  
 	     ( E m p t y ,   E m p t y )   - >   ( x ,   x )  
 	 |   ( E m p t y ,   _ )   - >    
 	         l e t   ( r _ m i n , r _ m a x )   =   a u x   r i g h t  
 	         i n   i f   x   <   r _ m i n   t h e n   ( x , r _ m a x )  
 	               e l s e   f a i l w i t h   " F a l s e "    
 	 |   ( _ ,   E m p t y )   - >    
 	         l e t   ( l _ m i n , l _ m a x )   =   a u x   l e f t  
 	         i n   i f   x   >   l _ m a x   t h e n   ( l _ m i n ,   x )  
 	               e l s e   f a i l w i t h   " F a l s e "    
 	 |   _   - >    
 	         l e t   ( ( l _ m i n , l _ m a x ) , ( r _ m i n ,   r _ m a x ) )   =   ( a u x   l e f t , a u x   r i g h t )    
 	         i n   i f   x   <   r _ m i n   & &   x   >   l _ m a x  
 	               t h e n   ( l _ m i n , r _ m a x )  
 	               e l s e   f a i l w i t h   " F a l s e "  
     i n    
     t r y   l e t   _   =   a u x   t r e e   i n   t r u e  
     w i t h   F a i l u r e   " E m p t y "   - >   t r u e  
           |   F a i l u r e   " F a l s e "   - >   f a l s e  
  
 ( * = = = = = = = = = = = =   E s 1 5 b : a b r - s e a r c h   = = = = = = = = = = = = = * )  
 ( *     a b r _ s e a r c h   :   ( ' a   *   ' b )   t r e e   - >   ' a   - >   ' b     * )  
 l e t   r e c   a b r _ s e a r c h   a b r   y   =    
     m a t c h   a b r   w i t h  
         E m p t y   - >   f a i l w i t h   " A B R   s e a r c h "  
     |   T r ( ( k , v ) , t 1 , t 2 )   - >    
             i f   k   =   y   t h e n   v  
             e l s e   i f   y   <   k   t h e n   a b r _ s e a r c h   t 1   y  
                       e l s e   a b r _ s e a r c h   t 2   y  
  
 ( * = = = = = = = = = = = =   E s 1 5 c : a b r - u p d a t e   = = = = = = = = = = = = = * )  
 ( *     a b r _ u p d a t e   :   ( ' a   *   ' b )   t r e e   - >   ' a   *   ' b   - >   ( ' a   *   ' b )   t r e e   * )  
 l e t   r e c   a b r _ u p d a t e   a b r   p a i r   =  
     l e t   ( k , v )   =   p a i r   i n    
     m a t c h   a b r   w i t h  
           E m p t y   - >   T r ( ( k , v ) , E m p t y , E m p t y )  
       |   T r ( ( k 1 , v 1 )   a s   x , t 1 , t 2 )   - >    
                   i f   k   =   k 1   t h e n   T r ( p a i r , t 1 , t 2 )  
                     e l s e   i f   k   <   k 1  
                               t h e n   T r ( x , a b r _ u p d a t e   t 1   p a i r ,   t 2 )  
                                 e l s e   T r ( x , t 1 , a b r _ u p d a t e   t 2   p a i r )  
  
 ( * = = = = = = = = = = = =   E s 1 5 d : a b r - d e l m i n   = = = = = = = = = = = = = * )  
 ( *   a b r _ d e l m i n   :   ' a   t r e e   - >   ' a   *   ' a   t r e e   * )  
 l e t   r e c   a b r _ d e l m i n   =   f u n c t i o n  
         E m p t y   - >   f a i l w i t h   " d e l m i n "  
     |   T r ( a , E m p t y , t )   - >   ( a , t )  
     |   T r ( a , l e f t , r i g h t )   - >    
                     l e t   ( x , n e w l e f t )   =   a b r _ d e l m i n   l e f t  
                     i n     ( x , T r ( a , n e w l e f t , r i g h t ) )  
  
 ( * = = = = = = = = = = = =   E s 1 5 e : a b r - d e l e t e   = = = = = = = = = = = = = * )  
  
 ( *   p e r   l a   c a n c e l l a z i o n e   d i   u n   n o d o ,   o c c o r r e   d e f i n i r e   u n a   f u n z i o n e  
       c h e   c a n c e l l i   l a   r a d i c e   d i   u n   a l b e r o ,   s o s t i t u e n d o l a   c o n   i l   " m i n i m o "  
       d e l   s u o   s o t t o a l b e r o   d e s t r o   * )  
 ( *   d e l r o o t   :   ' a   t r e e   - >   ' a   t r e e   * )  
 ( *   d e l r o o t   t   =   a l b e r o   c h e   s i   o t t i e n e   d a   t   s o s t i t u e n d o   l a   s u a   r a d i c e   c o n  
                               i l   m i n i m o   d e l   s u o   s o t t o a l b e r o   d e s t r o   ( c h e   v i e n e   e l i m i n a t o )   * )  
 l e t   r e c   d e l r o o t   =   f u n c t i o n  
         E m p t y   - >   f a i l w i t h   " d e l r o o t "  
     |   T r ( a , E m p t y , t 2 )   - >   t 2  
     |   T r ( a , t 1 , E m p t y )   - >   t 1  
     |   T r ( a , t 1 , t 2 )   - >   l e t   ( x , n e w r i g h t )   =   a b r _ d e l m i n   t 2  
                                       i n     T r ( x , t 1 , n e w r i g h t )  
  
 ( *   l a   f u n z i o n e   d i   c a n c e l l a z i o n e   a l l o r a   d e v e   c e r c a r e   i l   n o d o   d a  
       e l i m i n a r e   e   p o i   r i c h i a m a r e   d e l r o o t   s u l l ' a l b e r o   c h e   l o   h a  
       c o m e   r a d i c e   * )  
 ( *   a b r _ d e l e t e   :   ( ' a   *   ' b )   t r e e   - >   ' a   - >   ( ' a   *   ' b )   t r e e   * )  
 l e t   r e c   a b r _ d e l e t e   a b r   a   =    
     m a t c h   a b r   w i t h  
         E m p t y   - >   E m p t y  
     |   T r ( ( k , _ )   a s   b , t 1 , t 2 )   - >    
               i f   a   =   k   t h e n   d e l r o o t   ( T r ( b , t 1 , t 2 ) )  
               e l s e   i f   a   <   k  
                         t h e n   T r ( b , a b r _ d e l e t e   t 1   a , t 2 )  
                         e l s e   T r ( b , t 1 , a b r _ d e l e t e   t 2   a )  
  
  
 ( * = = = = = = = = = = = =   E s 1 5 f : t r e e - s o r t   = = = = = = = = = = = = = * )  
  
 ( *   v i s i t a   s i m m e t r i c a   * )  
 ( *   v e r s i o n e   c h e   e v i t a   l ' a p p e n d   a   o g n i   c h i a m a t a   r i c o r s i v a   * )  
 ( *   i n o r d   :   ' a   t r e e   - >   ' a   l i s t   * )  
 l e t   i n o r d   t   =  
     l e t   r e c   i n o r d t o   r e s u l t   =   f u n c t i o n  
             E m p t y   - >   r e s u l t  
         |   T r ( x , t 1 , t 2 )   - >   i n o r d t o   ( x : : ( i n o r d t o   r e s u l t   t 2 ) )   t 1  
     i n   i n o r d t o   [ ]   t  
  
 ( *     a b r _ i n s e r t   :   ' a   t r e e   - >   ' a   - >   ' a   t r e e   * )  
 l e t   r e c   a b r _ i n s e r t   a b r   y   =  
     m a t c h   a b r   w i t h  
           E m p t y   - >   T r ( y , E m p t y , E m p t y )  
       |   T r ( x , t 1 , t 2 )   - >    
                 i f   y   < =   x  
                 t h e n   T r ( x , a b r _ i n s e r t   t 1   y ,   t 2 )  
                 e l s e   T r ( x , t 1 , a b r _ i n s e r t   t 2   y )  
  
 ( *   c o s t r u z i o n e   d i   u n   A B R   d a   u n a   l i s t a   * )  
 ( *   f r o m _ l i s t   :   ' a   l i s t   - >   ' a   t r e e    
       f r o m _ l i s t   l s t   =   A B R   i   c u i   n o d i   s o n o   e t i c h e t t a t i   d a g l i   e l e m e n t i   d i   l s t   * )  
 l e t   r e c   f r o m _ l i s t   =   f u n c t i o n  
         [ ]   - >   E m p t y  
     |   x : : r e s t   - >  
             a b r _ i n s e r t   ( f r o m _ l i s t   r e s t )   x  
  
 ( *   t r e e _ s o r t :   ' a   l i s t   - >   ' a   l i s t   * )  
 l e t   t r e e _ s o r t   l s t   =  
     i n o r d   ( f r o m _ l i s t   l s t )  
 