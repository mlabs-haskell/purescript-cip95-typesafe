export const mkApi = eff =>
  new Proxy(
    {},
    {
      get(target, prop, receive) {
        if (prop === "cip95") {
          return (
            new Proxy (
              {},
              {
                get () {
                  return eff;
                }
              }
            )
          )
        }
        return eff;
      }
    }
  );

export const throwingApi = obj =>
  new Proxy(
    {},
    {
      get(target, prop, receive) {
        if (prop === "cip95") {
          return (
            new Proxy (
              {},
              {
                get () {
                  return () => {
                    throw obj;
                  };
                }
              }
            )
          )
        }
        return () => {
          throw obj;
        };
      }
    }
  );
