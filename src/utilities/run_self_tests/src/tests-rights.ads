package Tests.Rights is

   function Rights_Test return Boolean;
   --  Launch the rights broker with the clock as the authority that it
   --  holds, and check its policy and its grants (ADR 0021): a request
   --  is refused without a policy, and under sudo only if the policy says
   --  elevate; a grant works until its user finishes it; another user
   --  can't finish it; and the most specific policy wins.

end Tests.Rights;
