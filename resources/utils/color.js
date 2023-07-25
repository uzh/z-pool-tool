import crypto from 'crypto';

export const generateColor = (str) => `#${crypto.createHash('md5')
    .update(str)
    .digest('hex')
    .slice(0, 6)}`;
