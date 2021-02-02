# What it is

This is a sample application that leverages [User Long Poll API](https://vk.com/dev/using_longpoll) from VK social network implemented in Haskell. Since there is no VK SDK for Haskell language, all the communication with VK API is done "manually" by sending the messages to VK REST API and parsing the results.

The application can receive the messages via Long Poll API whenever a user writes anything to your chat bot.

## For whom it can be helpful

The project is probably an interesting example of JSON parsing tricks in the case when the sender can't guarantee the structure beforehand (e.g. "updates" field is said to be an array of arrays, the length of which can differ for different types of events being reported and the types of values in the array can also be different). In the light of such constraints such cool things like generic instances (i.e. when JSON parsing boilerplate code gets generated automatically) doesn't help much.

## Useful links

1. Wonderful Aeson tutorial from Artyom Kazak: https://artyom.me/aeson
2. Haskell Lens Operator Onboarding: https://medium.com/@russmatney/haskell-lens-operator-onboarding-a235481e8fac
3. What I wish I knew when learning Haskell http://dev.stephendiehl.com/hask/
