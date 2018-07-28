///
/// STATE
///
/// Allow an object to alter its behavior when its internal state changes. The object will appear to
/// change its class.
///

///
/// CONCLUSION
/// 
/// Functional programming is stateless. It should be no surprise that State design pattern is not
/// well fitted to functional programming.
/// 
/// State machines, state monads and folds may be used to manage states in functional programming.
/// However, none of these feel well suited to address the original intent of the design pattern.
/// 


///
/// NOTE
///
/// By nature functional program is immutable. State has to come from the outside of the pure
/// functional core logic.
///
/// For example infinite sequences may present incoming data. A fold function transforms sequence of
/// elements and initial state to a final state.
///

///
/// Seems that State monad is just another presentation for fold. In State monad the folder
/// function's signature is a -> State s () where as it is a -> b -> in fold.
///
